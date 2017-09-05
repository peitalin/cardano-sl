module Test.Pos.Wallet.Web.Methods.LogicSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec                   (Spec, describe)
import           Test.Hspec.QuickCheck        (prop)

--import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)

import           Pos.Wallet.Web.Methods.Logic (getAccounts, getWallets)

import           Test.Pos.Util                (giveCoreConf, giveGtConf, giveInfraConf,
                                               giveNodeConf, giveUpdateConf, stopProperty)
import           Test.Pos.Wallet.Web.Mode     (HasWalletSpecConfiguration, WalletProperty)
import           Test.Pos.Wallet.Web.Util     (wpGenBlocks)


spec :: Spec
spec = giveGtConf $ giveNodeConf $ giveInfraConf $ giveUpdateConf $ giveCoreConf $ do
    describe "Pos.Wallet.Web.Methods" $ do
        prop emptyWalletOnStarts emptyWallet
  where
    emptyWalletOnStarts = "wallet must be empty on start"

emptyWallet :: HasWalletSpecConfiguration => WalletProperty ()
emptyWallet = do
    wallets <- lift getWallets
    unless (null wallets) $
        stopProperty "Wallets aren't empty"
    accounts <- lift $ getAccounts Nothing
    unless (null accounts) $
        stopProperty "Accounts aren't empty"
