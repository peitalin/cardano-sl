-- |

module Test.Pos.Wallet.Web.Util
       ( wpGenBlocks
       , wpGenBlock
       ) where

import           Universum
import           Unsafe                      (unsafeHead)

import           Control.Monad.Random.Strict (evalRandT)

import qualified Data.List.NonEmpty          as NE
import           Pos.Block.Core              (blockHeader, blockHeaderHash)
import           Pos.Block.Types             (Blund)
import           Pos.Core                    (BlockCount, GenesisData (..),
                                              HasConfiguration, SlotId (..), epochIndexL,
                                              genesisData, headerHashG)
import           Pos.StateLock               (Priority (..), modifyStateLock)
import           Pos.Util.Chrono             (NE, OldestFirst (..))
import           Pos.Util.Util               (HasLens (..), _neLast)
import           Pos.Wallet.SscType          (WalletSscType)

import           Test.Pos.Block.Logic.Mode   (BlockProperty, BlockTestContext,
                                              BlockTestContextTag,
                                              HasVarSpecConfigurations, btcSlotIdL,
                                              tpAllSecrets)
import           Test.Pos.Block.Logic.Util   (EnableTxPayload, InplaceDB, bpGenBlock,
                                              bpGenBlocks)
import           Test.Pos.Wallet.Web.Mode    (WalletProperty, wtcBlockTestContext)

wpGenBlocks
    :: HasVarSpecConfigurations
    => Maybe BlockCount
    -> EnableTxPayload
    -> InplaceDB
    -> WalletProperty (OldestFirst [] (Blund WalletSscType))
wpGenBlocks blkCnt enTxPayload inplaceDB = undefined
    -- modifyStateLock HighPriority "wpGenBlocks" $ \curTip -> do
    --     blunds <- undefined --  local wtcBlockTestContext (bpGenBlocks blkCnt enTxPayload inplaceDB)
    --     let newTip = fromMaybe curTip $ view (_1 . headerHashG) <$> lastMay (getOldestFirst blunds)
    --     pure (newTip, blunds)

wpGenBlock
    :: HasVarSpecConfigurations
    => EnableTxPayload
    -> InplaceDB
    -> WalletProperty (Blund WalletSscType)
wpGenBlock = fmap (unsafeHead . toList) ... wpGenBlocks (Just 1)

