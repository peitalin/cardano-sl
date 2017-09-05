{-# LANGUAGE CPP             #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

-- | Execution modes for block logic tests.

module Test.Pos.Block.Logic.Mode
       ( TestParams (..)
       , HasTestParams (..)
       , TestInitModeContext (..)
       , BlockTestContextTag
       , PureDBSnapshotsVar(..)
       , BlockTestContext(..)
       , BlockTestMode
       , runBlockTestMode
       , initBlockTestContext

       , BlockProperty
       , blockPropertyToProperty

       , HasVarSpecConfigurations

       -- Lens
       , btcGStateL
       , btcSystemStartL
       , btcLoggerNameL
       , btcSSlottingVarL
       , btcUpdateContextL
       , btcSscStateL
       , btcTxpMemL
       , btcTxpGlobalSettingsL
       , btcSlotIdL
       , btcParamsL
       , btcReportingContextL
       , btcDelegationL
       , btcPureDBSnapshotsL

       -- MonadSlots
       , getCurrentSlotTestDefault
       , getCurrentSlotBlockingTestDefault
       , getCurrentSlotInaccurateTestDefault
       , currentTimeSlottingTestDefault
       ) where

import           Universum

import           Control.Lens                     (lens, makeClassy, makeLensesFor,
                                                   makeLensesWith)
import qualified Data.Map                         as Map
import qualified Data.Text.Buildable
import           Data.Time.Units                  (Microsecond, TimeUnit (..))
import           Ether.Internal                   (HasLens (..))
import           Formatting                       (bprint, build, formatToString, shown,
                                                   (%))
import           Mockable                         (Production, currentTime, runProduction)
import qualified Prelude
import           System.Wlog                      (HasLoggerName (..), LoggerName)
import           Test.QuickCheck                  (Arbitrary (..), Gen, Property,
                                                   Testable (..), forAll, ioProperty)
import           Test.QuickCheck.Monadic          (PropertyM, monadic)

import           Pos.AllSecrets                   (AllSecrets (..), HasAllSecrets (..),
                                                   mkAllSecretsSimple)
import           Pos.Block.BListener              (MonadBListener (..), onApplyBlocksStub,
                                                   onRollbackBlocksStub)
import           Pos.Block.Core                   (Block, BlockHeader)
import           Pos.Block.Slog                   (HasSlogGState (..), mkSlogGState)
import           Pos.Block.Types                  (Undo)
import           Pos.Configuration                (HasNodeConfiguration)
import           Pos.Core                         (HasConfiguration, IsHeader, SlotId,
                                                   Timestamp (..), genesisSecretKeys)
import           Pos.DB                           (DBPure, MonadBlockDBGeneric (..),
                                                   MonadBlockDBGenericWrite (..),
                                                   MonadDB (..), MonadDBRead (..),
                                                   MonadGState (..))
import qualified Pos.DB                           as DB
import qualified Pos.DB.Block                     as DB
import           Pos.DB.DB                        (gsAdoptedBVDataDefault, initNodeDBs)
import           Pos.DB.Pure                      (DBPureVar, newDBPureVar)
import           Pos.Delegation                   (DelegationVar, mkDelegationVar)
import           Pos.Generator.BlockEvent         (SnapshotId)
import qualified Pos.GState                       as GS
import           Pos.Infra.Configuration          (HasInfraConfiguration)
import           Pos.KnownPeers                   (MonadFormatPeers (..))
import           Pos.Lrc                          (LrcContext (..), mkLrcSyncData)
import           Pos.Reporting                    (HasReportingContext (..),
                                                   ReportingContext,
                                                   emptyReportingContext)
import           Pos.Slotting                     (HasSlottingVar (..), MonadSlots (..),
                                                   SimpleSlottingMode, SimpleSlottingVar,
                                                   SlottingData,
                                                   currentTimeSlottingSimple,
                                                   getCurrentSlotBlockingSimple,
                                                   getCurrentSlotInaccurateSimple,
                                                   getCurrentSlotSimple,
                                                   mkSimpleSlottingVar)
import           Pos.Slotting.MemState            (MonadSlotsData)
import           Pos.Ssc.Class                    (SscBlock)
import           Pos.Ssc.Class.Helpers            (SscHelpersClass)
import           Pos.Ssc.Extra                    (SscMemTag, SscState, mkSscState)
import           Pos.Ssc.GodTossing               (SscGodTossing)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Txp                          (GenericTxpLocalData, TxpGlobalSettings,
                                                   TxpHolderTag, mkTxpLocalData)
import           Pos.Update.Configuration         (HasUpdateConfiguration)
import           Pos.Update.Context               (UpdateContext, mkUpdateContext)
import           Pos.Util                         (Some, newInitFuture, postfixLFields)
import           Pos.Util.LoggerName              (HasLoggerName' (..),
                                                   getLoggerNameDefault,
                                                   modifyLoggerNameDefault)
import           Pos.WorkMode.Class               (TxpExtra_TMP)
#ifdef WITH_EXPLORER
import           Pos.Explorer                     (explorerTxpGlobalSettings)
#else
import           Pos.Txp                          (txpGlobalSettings)
#endif

import           Test.Pos.Block.Logic.Emulation   (Emulation (..), runEmulation,
                                                   sudoLiftIO)

type HasVarSpecConfigurations =
       ( HasNodeConfiguration
       , HasGtConfiguration
       , HasConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       )

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

-- | This data type contains all parameters which should be generated
-- before testing starts.
data TestParams = TestParams
    { _tpAllSecrets :: !AllSecrets
    -- ^ Secret keys corresponding to 'PubKeyAddress'es from
    -- genesis 'Utxo'.
    -- They are stored in map (with 'StakeholderId' as key) to make it easy
    -- to find 'SecretKey' corresponding to given 'StakeholderId'.
    -- In tests we often want to have inverse of 'hash' and 'toPublic'.
    , _tpStartTime  :: !Microsecond
    }

makeClassy ''TestParams

instance HasAllSecrets TestParams where
    allSecrets = tpAllSecrets

instance Buildable TestParams where
    build TestParams {..} =
        bprint ("TestParams {\n"%
                "  secrets: "%build%"\n"%
                "  start time: "%shown%"\n"%
                "}\n")
            _tpAllSecrets
            _tpStartTime

instance Show TestParams where
    show = formatToString build

instance HasConfiguration => Arbitrary TestParams where
    arbitrary = do
        keys <- case genesisSecretKeys of
            Nothing -> error "arbitrary @TestParams: no genesisSecretKeys"
            Just ks -> pure ks
        let _tpAllSecrets = mkAllSecretsSimple keys
            _tpStartTime  = fromMicroseconds 0
        return TestParams {..}

----------------------------------------------------------------------------
-- Init mode with instances
----------------------------------------------------------------------------

-- The fields are lazy on purpose: this allows using them with
-- futures.
data TestInitModeContext = TestInitModeContext
    { timcDBPureVar   :: DBPureVar
    , timcSlottingVar :: TVar SlottingData
    , timcSystemStart :: !Timestamp
    , timcLrcContext  :: LrcContext
    }

makeLensesWith postfixLFields ''TestInitModeContext

type TestInitMode = ReaderT TestInitModeContext Production

runTestInitMode :: TestInitModeContext -> TestInitMode a -> IO a
runTestInitMode ctx = runProduction . flip runReaderT ctx

----------------------------------------------------------------------------
-- Main context
----------------------------------------------------------------------------

newtype PureDBSnapshotsVar = PureDBSnapshotsVar
    { getPureDBSnapshotsVar :: IORef (Map SnapshotId DBPure)
    }

data BlockTestContext = BlockTestContext
    { btcGState            :: !GS.GStateContext
    , btcSystemStart       :: !Timestamp
    , btcLoggerName        :: !LoggerName
    , btcSSlottingVar      :: !SimpleSlottingVar
    , btcUpdateContext     :: !UpdateContext
    , btcSscState          :: !(SscState SscGodTossing)
    , btcTxpMem            :: !(GenericTxpLocalData TxpExtra_TMP)
    , btcTxpGlobalSettings :: !TxpGlobalSettings
    , btcSlotId            :: !(Maybe SlotId)
    -- ^ If this value is 'Just' we will return it as the current
    -- slot. Otherwise simple slotting is used.
    , btcParams            :: !TestParams
    , btcReportingContext  :: !ReportingContext
    , btcDelegation        :: !DelegationVar
    , btcPureDBSnapshots   :: !PureDBSnapshotsVar
    }

flip makeLensesFor ''BlockTestContext
    [ ("btcGState", "btcGStateL")
    , ("btcSystemStart", "btcSystemStartL")
    , ("btcLoggerName", "btcLoggerNameL")
    , ("btcSSlottingVar", "btcSSlottingVarL")
    , ("btcUpdateContext", "btcUpdateContextL")
    , ("btcSscState", "btcSscStateL")
    , ("btcTxpMem", "btcTxpMemL")
    , ("btcTxpGlobalSettings", "btcTxpGlobalSettingsL")
    , ("btcSlotId", "btcSlotIdL")
    , ("btcParams", "btcParamsL")
    , ("btcReportingContext", "btcReportingContextL")
    , ("btcDelegation", "btcDelegationL")
    , ("btcPureDBSnapshots", "btcPureDBSnapshotsL")
    ]

instance HasTestParams BlockTestContext where
    testParams = btcParamsL

instance HasAllSecrets BlockTestContext where
    allSecrets = testParams . allSecrets

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initBlockTestContext
    :: (HasConfiguration, HasGtConfiguration, HasNodeConfiguration)
    => TestParams
    -> (BlockTestContext -> Emulation a)
    -> Emulation a
initBlockTestContext tp@TestParams {..} callback = do
    clockVar <- Emulation ask
    dbPureVar <- newDBPureVar
    (futureLrcCtx, putLrcCtx) <- newInitFuture "lrcCtx"
    (futureSlottingVar, putSlottingVar) <- newInitFuture "slottingVar"
    systemStart <- Timestamp <$> currentTime
    let initCtx =
            TestInitModeContext
                dbPureVar
                futureSlottingVar
                systemStart
                futureLrcCtx
        initBlockTestContextDo = do
            initNodeDBs @SscGodTossing
            _gscSlottingVar <- newTVarIO =<< GS.getSlottingData
            putSlottingVar _gscSlottingVar
            btcSSlottingVar <- mkSimpleSlottingVar
            let btcLoggerName = "testing"
            lcLrcSync <- mkLrcSyncData >>= newTVarIO
            let _gscLrcContext = LrcContext {..}
            putLrcCtx _gscLrcContext
            btcUpdateContext <- mkUpdateContext
            btcSscState <- mkSscState @SscGodTossing
            _gscSlogGState <- mkSlogGState
            btcTxpMem <- mkTxpLocalData
#ifdef WITH_EXPLORER
            let btcTxpGlobalSettings = explorerTxpGlobalSettings
#else
            let btcTxpGlobalSettings = txpGlobalSettings
#endif
            let btcReportingContext = emptyReportingContext
            let btcSlotId = Nothing
            let btcParams = tp
            let btcGState = GS.GStateContext {_gscDB = DB.PureDB dbPureVar, ..}
            btcDelegation <- mkDelegationVar @SscGodTossing
            btcPureDBSnapshots <- PureDBSnapshotsVar <$> newIORef Map.empty
            let btCtx = BlockTestContext {btcSystemStart = systemStart, ..}
            liftIO $ flip runReaderT clockVar $ unEmulation $ callback btCtx
    sudoLiftIO $ runTestInitMode initCtx $ initBlockTestContextDo

----------------------------------------------------------------------------
-- ExecMode
----------------------------------------------------------------------------

data BlockTestContextTag

instance HasLens BlockTestContextTag BlockTestContext BlockTestContext where
    lensOf = identity

type BlockTestMode = ReaderT BlockTestContext Emulation

runBlockTestMode :: (HasNodeConfiguration, HasGtConfiguration, HasConfiguration)
                 => TestParams -> BlockTestMode a -> IO a
runBlockTestMode tp action =
    runEmulation (tp ^. tpStartTime) $
    initBlockTestContext tp (runReaderT action)

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type BlockProperty = PropertyM BlockTestMode

-- | Convert 'BlockProperty' to 'Property' using given generator of
-- 'TestParams'.
blockPropertyToProperty :: (HasNodeConfiguration, HasGtConfiguration, HasConfiguration)
                        => Gen TestParams -> BlockProperty a -> Property
blockPropertyToProperty tpGen blockProperty =
    forAll tpGen $ \tp ->
        monadic (ioProperty . runBlockTestMode tp) blockProperty

-- | 'Testable' instance allows one to write monadic properties in
-- do-notation and pass them directly to QuickCheck engine. It uses
-- arbitrary 'TestParams'. For more fine-grained control over
-- parameters use 'blockPropertyToProperty'.
instance (HasNodeConfiguration, HasGtConfiguration, HasConfiguration)
         => Testable (BlockProperty a) where
    property = blockPropertyToProperty arbitrary

----------------------------------------------------------------------------
-- Boilerplate TestInitContext instances
----------------------------------------------------------------------------

instance HasLens DBPureVar TestInitModeContext DBPureVar where
    lensOf = timcDBPureVar_L

instance HasLens LrcContext TestInitModeContext LrcContext where
    lensOf = timcLrcContext_L

instance HasSlottingVar TestInitModeContext where
    slottingTimestamp = timcSystemStart_L
    slottingVar = timcSlottingVar_L

instance HasConfiguration => MonadDBRead TestInitMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance HasConfiguration => MonadDB TestInitMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance
    (HasConfiguration, SscHelpersClass ssc) =>
    MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo TestInitMode
  where
    dbGetBlock  = DB.dbGetBlockPureDefault @ssc
    dbGetUndo   = DB.dbGetUndoPureDefault @ssc
    dbGetHeader = DB.dbGetHeaderPureDefault @ssc

instance (HasConfiguration, SscHelpersClass ssc) =>
         MonadBlockDBGenericWrite (BlockHeader ssc) (Block ssc) Undo TestInitMode where
    dbPutBlund = DB.dbPutBlundPureDefault

instance
    (HasConfiguration, SscHelpersClass ssc) =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () TestInitMode
  where
    dbGetBlock  = DB.dbGetBlockSscPureDefault @ssc
    dbGetUndo   = DB.dbGetUndoSscPureDefault @ssc
    dbGetHeader = DB.dbGetHeaderSscPureDefault @ssc

instance (HasConfiguration, MonadSlotsData ctx TestInitMode)
      => MonadSlots ctx TestInitMode
  where
    getCurrentSlot           = getCurrentSlotSimple           =<< mkSimpleSlottingVar
    getCurrentSlotBlocking   = getCurrentSlotBlockingSimple   =<< mkSimpleSlottingVar
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple =<< mkSimpleSlottingVar
    currentTimeSlotting      = currentTimeSlottingSimple

----------------------------------------------------------------------------
-- Boilerplate BlockTestContext instances
----------------------------------------------------------------------------

instance GS.HasGStateContext BlockTestContext where
    gStateContext = btcGStateL

instance HasLens DBPureVar BlockTestContext DBPureVar where
    lensOf = GS.gStateContext . GS.gscDB . pureDBLens
      where
        -- pva701: sorry for newbie code
        getter = \case
            DB.RealDB _   -> realDBInTestsError
            DB.PureDB pdb -> pdb
        setter _ pdb = DB.PureDB pdb
        pureDBLens = lens getter setter
        realDBInTestsError = error "You are using real db in tests"

instance HasLens PureDBSnapshotsVar BlockTestContext PureDBSnapshotsVar where
    lensOf = btcPureDBSnapshotsL

instance HasLens LoggerName BlockTestContext LoggerName where
      lensOf = btcLoggerNameL

instance HasLens LrcContext BlockTestContext LrcContext where
    lensOf = GS.gStateContext . GS.gscLrcContext

instance HasLens UpdateContext BlockTestContext UpdateContext where
      lensOf = btcUpdateContextL

instance HasLens SscMemTag BlockTestContext (SscState SscGodTossing) where
      lensOf = btcSscStateL

instance HasLens TxpGlobalSettings BlockTestContext TxpGlobalSettings where
      lensOf = btcTxpGlobalSettingsL

instance HasLens TestParams BlockTestContext TestParams where
      lensOf = btcParamsL

instance HasLens SimpleSlottingVar BlockTestContext SimpleSlottingVar where
      lensOf = btcSSlottingVarL

instance HasReportingContext BlockTestContext where
    reportingContext = btcReportingContextL

instance HasSlottingVar BlockTestContext where
    slottingTimestamp = btcSystemStartL
    slottingVar = GS.gStateContext . GS.gscSlottingVar

instance HasSlogGState BlockTestContext where
    slogGState = GS.gStateContext . GS.gscSlogGState

instance HasLens DelegationVar BlockTestContext DelegationVar where
    lensOf = btcDelegationL

instance HasLens TxpHolderTag BlockTestContext (GenericTxpLocalData TxpExtra_TMP) where
    lensOf = btcTxpMemL

instance HasLoggerName' BlockTestContext where
    loggerName = lensOf @LoggerName

instance {-# OVERLAPPING #-} HasLoggerName BlockTestMode where
    getLoggerName = getLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

type TestSlottingContext ctx m =
    ( SimpleSlottingMode ctx m
    , HasLens BlockTestContext ctx BlockTestContext
    )

testSlottingHelper
    :: TestSlottingContext ctx m
    => (SimpleSlottingVar -> m a)
    -> (SlotId -> a)
    -> m a
testSlottingHelper targetF alternative = do
    BlockTestContext{..} <- view (lensOf @BlockTestContext)
    case btcSlotId of
        Nothing   -> targetF btcSSlottingVar
        Just slot -> pure $ alternative slot

getCurrentSlotTestDefault :: TestSlottingContext ctx m => m (Maybe SlotId)
getCurrentSlotTestDefault = testSlottingHelper getCurrentSlotSimple Just

getCurrentSlotBlockingTestDefault :: TestSlottingContext ctx m => m SlotId
getCurrentSlotBlockingTestDefault = testSlottingHelper getCurrentSlotBlockingSimple identity

getCurrentSlotInaccurateTestDefault :: TestSlottingContext ctx m => m SlotId
getCurrentSlotInaccurateTestDefault = testSlottingHelper getCurrentSlotInaccurateSimple identity

currentTimeSlottingTestDefault :: SimpleSlottingMode ctx m => m Timestamp
currentTimeSlottingTestDefault = currentTimeSlottingSimple

instance (HasConfiguration, MonadSlotsData ctx BlockTestMode)
        => MonadSlots ctx BlockTestMode where
    getCurrentSlot = getCurrentSlotTestDefault
    getCurrentSlotBlocking = getCurrentSlotBlockingTestDefault
    getCurrentSlotInaccurate = getCurrentSlotInaccurateTestDefault
    currentTimeSlotting = currentTimeSlottingTestDefault

instance HasConfiguration => MonadDBRead BlockTestMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance HasConfiguration => MonadDB BlockTestMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance HasConfiguration =>
         MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo BlockTestMode
  where
    dbGetBlock = DB.dbGetBlockPureDefault
    dbGetUndo = DB.dbGetUndoPureDefault @SscGodTossing
    dbGetHeader = DB.dbGetHeaderPureDefault @SscGodTossing

instance HasConfiguration => MonadBlockDBGeneric (Some IsHeader) (SscBlock SscGodTossing) () BlockTestMode
  where
    dbGetBlock = DB.dbGetBlockSscPureDefault
    dbGetUndo = DB.dbGetUndoSscPureDefault @SscGodTossing
    dbGetHeader = DB.dbGetHeaderSscPureDefault @SscGodTossing

instance HasConfiguration =>
         MonadBlockDBGenericWrite (BlockHeader SscGodTossing) (Block SscGodTossing) Undo BlockTestMode where
    dbPutBlund = DB.dbPutBlundPureDefault

instance HasConfiguration => MonadGState BlockTestMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadBListener BlockTestMode where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance MonadFormatPeers BlockTestMode where
    formatKnownPeers _ = pure Nothing
