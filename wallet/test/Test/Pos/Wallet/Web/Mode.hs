{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Module which provides `MonadWalletWebMode` instance for tests

module Test.Pos.Wallet.Web.Mode
       ( WalletTestParams (..)
       , HasWalletTestParams (..)
       , WalletTestMode
       , WalletTestContext (..)
       , runWalletTestMode
       , WalletProperty
       , HasWalletSpecConfiguration
       ) where

import           Universum

import qualified Control.Concurrent.STM            as STM
import           Control.Lens                      (lens, makeClassy, makeLensesWith)
--import           Control.Monad.Catch               (MonadMask (..))
import qualified Data.Text.Buildable
import           Ether.Internal                    (HasLens (..))
import           Formatting                        (bprint, build, formatToString, (%))
import qualified Prelude
import           System.Wlog                       (HasLoggerName (..), LoggerName)
import           Test.QuickCheck                   (Arbitrary (..), Property,
                                                    Testable (..), forAll, ioProperty)
import           Test.QuickCheck.Gen               (Gen)
import           Test.QuickCheck.Monadic           (PropertyM, monadic)

import           Pos.AllSecrets                    (HasAllSecrets (..))
import           Pos.Block.BListener               (MonadBListener (..))
import           Pos.Block.Core                    (Block, BlockHeader)
import           Pos.Block.Types                   (Undo)
import           Pos.Client.Txp.Addresses          (MonadAddresses (..))
import           Pos.Client.Txp.Balances           (MonadBalances (..), getBalanceDefault,
                                                    getOwnUtxosDefault)
import           Pos.Client.Txp.History            (MonadTxHistory (..),
                                                    getBlockHistoryDefault,
                                                    getLocalHistoryDefault, saveTxDefault)
import           Pos.Configuration                 (HasNodeConfiguration)
import           Pos.Context                       (ConnectedPeers (..), LastKnownHeader,
                                                    LastKnownHeaderTag, ProgressHeader,
                                                    ProgressHeaderTag, RecoveryHeader,
                                                    RecoveryHeaderTag)
import           Pos.Core                          (HasConfiguration, IsHeader)
import           Pos.Crypto                        (PassPhrase)
import           Pos.DB                            (MonadBlockDBGeneric (..),
                                                    MonadDBRead (..), MonadGState (..))
import qualified Pos.DB                            as DB
import qualified Pos.DB.Block                      as DB
import           Pos.DB.DB                         (gsAdoptedBVDataDefault)
import           Pos.DB.Pure                       (DBPureVar)
import qualified Pos.GState                        as GS
import           Pos.Infra.Configuration           (HasInfraConfiguration)
import           Pos.KnownPeers                    (MonadFormatPeers (..),
                                                    MonadKnownPeers (..))
import           Pos.Reporting                     (HasReportingContext (..))
import           Pos.Shutdown                      (HasShutdownContext (..),
                                                    ShutdownContext (..))
import           Pos.Slotting                      (HasSlottingVar (..), MonadSlots (..),
                                                    MonadSlotsData)
import           Pos.Ssc.Class                     (SscBlock)
import           Pos.Ssc.GodTossing.Configuration  (HasGtConfiguration)
import           Pos.StateLock                     (StateLock, StateLockMetrics (..),
                                                    newStateLock)
import           Pos.Txp                           (GenericTxpLocalData, TxpHolderTag)
import           Pos.Update.Configuration          (HasUpdateConfiguration)
import           Pos.Update.Context                (UpdateContext)
import           Pos.Util.JsonLog                  (HasJsonLogConfig (..),
                                                    JsonLogConfig (..), jsonLogDefault)
import           Pos.Util.LoggerName               (HasLoggerName' (..),
                                                    getLoggerNameDefault,
                                                    modifyLoggerNameDefault)
import           Pos.Util.TimeWarp                 (CanJsonLog (..))
import           Pos.Util.UserSecret               (UserSecret)
import           Pos.Util.Util                     (Some, postfixLFields)
import           Pos.Wallet.Redirect               (applyLastUpdateWebWallet,
                                                    blockchainSlotDurationWebWallet,
                                                    connectedPeersWebWallet,
                                                    localChainDifficultyWebWallet,
                                                    networkChainDifficultyWebWallet,
                                                    waitForUpdateWebWallet)
import           Pos.WorkMode.Class                (TxpExtra_TMP)

import           Pos.Wallet.KeyStorage             (MonadKeys (..))
import           Pos.Wallet.SscType                (WalletSscType)
import           Pos.Wallet.WalletMode             (MonadBlockchainInfo (..),
                                                    MonadUpdates (..))
import           Pos.Wallet.Web.ClientTypes        (AccountId)
import           Pos.Wallet.Web.Methods.Payment    (getNewAddressWebWallet)
import           Pos.Wallet.Web.State              (WalletState, openMemState)
import           Pos.Wallet.Web.Tracking.BListener (onApplyBlocksWebWallet,
                                                    onRollbackBlocksWebWallet)

import           Test.Pos.Block.Logic.Emulation    (Emulation (..), runEmulation)
import           Test.Pos.Block.Logic.Mode         (BlockTestContext (..),
                                                    HasTestParams (..), TestParams (..),
                                                    btcReportingContextL, btcSystemStartL,
                                                    btcTxpMemL,
                                                    currentTimeSlottingTestDefault,
                                                    getCurrentSlotBlockingTestDefault,
                                                    getCurrentSlotInaccurateTestDefault,
                                                    getCurrentSlotTestDefault,
                                                    initBlockTestContext)

type HasWalletSpecConfiguration
   = ( HasConfiguration
     , HasGtConfiguration
     , HasInfraConfiguration
     , HasUpdateConfiguration
     , HasNodeConfiguration
     )

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

-- | This datatype contains all parameters which should be generated before
-- testing starts
data WalletTestParams = WalletTestParams
    { _wtpBlockTestParams :: !TestParams
    -- ^ Block test params
    -- TODO: add wallet-specific parameters
    }

makeClassy ''WalletTestParams

instance HasAllSecrets WalletTestParams where
    allSecrets = wtpBlockTestParams . allSecrets

instance Buildable WalletTestParams where
    build WalletTestParams {..} =
        bprint ("WalletTestParams {\n"%
                "  blockTestParams = "%build%"\n"%
                "}\n")
        _wtpBlockTestParams

instance Show WalletTestParams where
    show = formatToString build

instance (HasConfiguration, HasNodeConfiguration) => Arbitrary WalletTestParams where
    arbitrary = WalletTestParams <$> arbitrary

----------------------------------------------------------------------------
-- Wallet context
----------------------------------------------------------------------------

data WalletTestContext = WalletTestContext
    { wtcBlockTestContext :: !BlockTestContext
    , wtcWalletState      :: !WalletState
    -- , wtcStateLock           :: !StateLock
    -- ^ A lock which manages access to shared resources.
    -- Stored hash is a hash of last applied block.
    , wtcUserSecret       :: !(TVar UserSecret)
    -- ^ Secret keys which are used to send transactions
    , wtcRecoveryHeader   :: !(RecoveryHeader WalletSscType)
    -- ^ Stub empty value, not used for tests for now.
    , wtcProgressHeader   :: !(ProgressHeader WalletSscType)
    , wtcLastKnownHeader  :: !(LastKnownHeader WalletSscType)
    , wtcStateLock        :: !StateLock
    , wtcShutdownContext  :: !ShutdownContext
    -- Stub
    , wtcConnectedPeers   :: !ConnectedPeers
    -- Stub
    }

makeLensesWith postfixLFields ''WalletTestContext

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initWalletTestContext
    :: (HasConfiguration, HasGtConfiguration, HasNodeConfiguration)
    => WalletTestParams
    -> (WalletTestContext -> Emulation a)
    -> Emulation a
initWalletTestContext WalletTestParams {..} callback =
    initBlockTestContext _wtpBlockTestParams $ \wtcBlockTestContext -> do
        wtcWalletState <- openMemState
        wtcUserSecret <- undefined
        wtcRecoveryHeader <- liftIO STM.newEmptyTMVarIO
        wtcStateLock <- newStateLock =<< undefined
        wtcShutdownContext <- liftIO $ ShutdownContext <$> STM.newTVarIO False
        wtcConnectedPeers <- liftIO $ ConnectedPeers <$> STM.newTVarIO mempty
        wtcProgressHeader <- liftIO STM.newEmptyTMVarIO
        wtcLastKnownHeader <- liftIO $ STM.newTVarIO Nothing
        callback $ WalletTestContext {..}

type WalletTestMode = ReaderT WalletTestContext Emulation

runWalletTestMode
    :: (HasConfiguration, HasGtConfiguration, HasNodeConfiguration)
    => WalletTestParams
    -> WalletTestMode a
    -> IO a
runWalletTestMode wtp action =
    runEmulation (wtp ^. wtpBlockTestParams . tpStartTime) $
    initWalletTestContext wtp $
    runReaderT action

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

-- WalletProperty is similar to BlockProperty from Test.Pos.Block.Logic.Mode.

type WalletProperty = PropertyM WalletTestMode

-- | Convert 'WalletProperty' to 'Property' using given generator of
-- 'WalletTestParams'.
walletPropertyToProperty
    :: (HasConfiguration, HasGtConfiguration, HasNodeConfiguration)
    => Gen WalletTestParams
    -> WalletProperty a
    -> Property
walletPropertyToProperty wtpGen walletProperty =
    forAll wtpGen $ \wtp ->
        monadic (ioProperty . runWalletTestMode wtp) walletProperty

instance (HasConfiguration, HasGtConfiguration, HasNodeConfiguration)
        => Testable (WalletProperty a) where
    property = walletPropertyToProperty arbitrary

----------------------------------------------------------------------------
-- Instances derived from BlockTestContext
----------------------------------------------------------------------------

instance HasLens BlockTestContext WalletTestContext BlockTestContext where
    lensOf = wtcBlockTestContext_L

instance HasLens DBPureVar WalletTestContext DBPureVar where
    lensOf = wtcBlockTestContext_L . lensOf @DBPureVar

instance HasSlottingVar WalletTestContext where
    slottingTimestamp = wtcBlockTestContext_L . btcSystemStartL
    slottingVar = wtcBlockTestContext_L . GS.gStateContext . GS.gscSlottingVar

instance (HasConfiguration, MonadSlotsData ctx WalletTestMode)
        => MonadSlots ctx WalletTestMode where
    getCurrentSlot = getCurrentSlotTestDefault
    getCurrentSlotBlocking = getCurrentSlotBlockingTestDefault
    getCurrentSlotInaccurate = getCurrentSlotInaccurateTestDefault
    currentTimeSlotting = currentTimeSlottingTestDefault

instance HasLens UpdateContext WalletTestContext UpdateContext where
      lensOf = wtcBlockTestContext_L . lensOf @UpdateContext

instance HasReportingContext WalletTestContext where
    reportingContext = wtcBlockTestContext_L . btcReportingContextL

instance HasJsonLogConfig WalletTestContext where
    jsonLogConfig = lens (const JsonLogDisabled) const

instance {-# OVERLAPPING #-} CanJsonLog WalletTestMode where
    jsonLog = jsonLogDefault

instance HasLoggerName' WalletTestContext where
    loggerName = wtcBlockTestContext_L . lensOf @LoggerName

instance HasLens TxpHolderTag WalletTestContext (GenericTxpLocalData TxpExtra_TMP) where
    lensOf = wtcBlockTestContext_L . btcTxpMemL

instance {-# OVERLAPPING #-} HasLoggerName WalletTestMode where
    getLoggerName = getLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance HasConfiguration => MonadDBRead WalletTestMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance HasConfiguration =>
         MonadBlockDBGeneric (BlockHeader WalletSscType) (Block WalletSscType) Undo WalletTestMode
  where
    dbGetBlock = DB.dbGetBlockPureDefault
    dbGetUndo = DB.dbGetUndoPureDefault @WalletSscType
    dbGetHeader = DB.dbGetHeaderPureDefault @WalletSscType

instance HasConfiguration => MonadBlockDBGeneric (Some IsHeader) (SscBlock WalletSscType) () WalletTestMode
  where
    dbGetBlock = DB.dbGetBlockSscPureDefault
    dbGetUndo = DB.dbGetUndoSscPureDefault @WalletSscType
    dbGetHeader = DB.dbGetHeaderSscPureDefault @WalletSscType

instance HasConfiguration => MonadGState WalletTestMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadFormatPeers WalletTestMode where
    formatKnownPeers _ = pure Nothing

instance MonadKnownPeers WalletTestMode where
    updatePeersBucket _ _ = pure True

----------------------------------------------------------------------------
-- Wallet instances
----------------------------------------------------------------------------

instance HasLens WalletState WalletTestContext WalletState where
    lensOf = wtcWalletState_L

-- For MonadUpdates
instance HasShutdownContext WalletTestContext where
    shutdownContext = wtcShutdownContext_L

-- For MonadRecoveryInfo
instance HasLens RecoveryHeaderTag WalletTestContext (RecoveryHeader WalletSscType) where
    lensOf = wtcRecoveryHeader_L

-- For BListener
instance HasLens StateLock WalletTestContext StateLock where
    lensOf = wtcStateLock_L

-- For MonadBlockchainInfo
instance HasLens LastKnownHeaderTag WalletTestContext (LastKnownHeader WalletSscType) where
    lensOf = wtcLastKnownHeader_L

instance HasLens ProgressHeaderTag WalletTestContext (ProgressHeader WalletSscType) where
    lensOf = wtcProgressHeader_L

instance HasLens ConnectedPeers WalletTestContext ConnectedPeers where
    lensOf = wtcConnectedPeers_L

-- TODO may be used for callback on tx processing in future.
instance HasLens StateLockMetrics WalletTestContext StateLockMetrics where
    lensOf = lens (const emptyStateMetrics) const
      where
        emptyStateMetrics = StateLockMetrics
            { slmWait = const $ pure ()
            , slmAcquire = const $ pure ()
            , slmRelease = const $ pure ()
            }

instance
    ( HasConfiguration
    , HasNodeConfiguration
    , HasInfraConfiguration
    , HasGtConfiguration
    , HasUpdateConfiguration
    )
    => MonadAddresses WalletTestMode where
    type AddrData WalletTestMode = (AccountId, PassPhrase)
    getNewAddress = getNewAddressWebWallet

instance MonadKeys WalletTestMode where
    getSecret = view wtcUserSecret_L >>= atomically . STM.readTVar
    modifySecret f = do
        us <- view wtcUserSecret_L
        void $ atomically $ STM.modifyTVar us f

instance HasConfiguration => MonadTxHistory WalletSscType WalletTestMode where
    getBlockHistory = getBlockHistoryDefault @WalletSscType
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance HasConfiguration => MonadBalances WalletTestMode where
    getOwnUtxos = getOwnUtxosDefault
    getBalance = getBalanceDefault

instance MonadUpdates WalletTestMode where
    waitForUpdate = waitForUpdateWebWallet
    applyLastUpdate = applyLastUpdateWebWallet

instance HasConfiguration => MonadBListener WalletTestMode where
    onApplyBlocks = onApplyBlocksWebWallet
    onRollbackBlocks = onRollbackBlocksWebWallet

instance HasConfiguration => MonadBlockchainInfo WalletTestMode where
    networkChainDifficulty = networkChainDifficultyWebWallet
    localChainDifficulty = localChainDifficultyWebWallet
    blockchainSlotDuration = blockchainSlotDurationWebWallet
    connectedPeers = connectedPeersWebWallet
