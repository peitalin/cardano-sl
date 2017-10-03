#!/usr/bin/env stack
-- stack runghc --package vector --package cassava --package turtle --package safe --package aeson --package yaml --package lens-aeson

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

import           Control.Lens
import           Control.Monad             (forM_, void, when)
import           Control.Monad.Except      (ExceptT (..), runExceptT)
import           Control.Monad.Trans       (lift)
import           Data.Aeson.Lens
import qualified Data.List                 as L
import qualified Data.Map                  as M
import           Data.Maybe                (catMaybes, fromJust, fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Scientific           (Scientific)
import qualified Data.Text                 as T
import           Data.Text.IO              as TIO
import           Data.Text.Lazy            (fromStrict)
import           Data.Text.Lazy.Encoding   (encodeUtf8)
import qualified Data.Yaml                 as Y
import           Filesystem.Path.CurrentOS (encodeString)
import qualified Filesystem.Path.CurrentOS as F
import           GHC.Generics
import           Prelude                   hiding (FilePath)
import           Text.Printf
import           Turtle                    hiding (printf)


-- CLI Parser

data Command =
    Update
  | GenWalletConf !FilePath
  deriving (Show)

cmdParser :: Parser Command
cmdParser =
      subcommand "update" "Test software update" (pure Update)
  <|> subcommand "gen-wallet-conf" "generate wallet configuration"
        (GenWalletConf <$> optPath "out" 'o' "Output file")

data Config = Config
      { cFile :: FilePath
      , cKey  :: Text
      , cCmd  :: Command
      }

parser :: Parser Config
parser = Config
    <$> optPath "configuration-file" 'c' "Configuration file"
    <*> optText "configuration-key" 'k' "Configuration key"
    <*> cmdParser

walletAppName :: Text
walletAppName = "csl-daedalus"

main :: IO ()
main = do
  c@Config {..} <- options "Helper CLI around NixOps to run experiments" parser
  config <- fromMaybe (error "No config key found") . M.lookup cKey . either (error "Wrong yaml config") id
              <$> Y.decodeFileEither (F.encodeString cFile)
  case cCmd of
    Update -> updateTest config c
    GenWalletConf outFile ->
        let config' = config & appNameL .~ walletAppName
         in Y.encodeFile (F.encodeString outFile) (M.singleton cKey config')

toObject :: Y.Value -> Maybe Y.Object
toObject (Y.Object a) = Just a
toObject _            = Nothing

genSpecL :: AsValue t => Traversal' t Y.Value
genSpecL = key "core"
         . key "genesis"
         . key "spec"

genesisConstL = genSpecL
         . key "blockVersionData"

genSlotL = genesisConstL
         . key "slotDuration"
         . _Number

genBlockSizeL = genesisConstL
         . key "maxBlockSize"
         . _Number

genScriptVersionL = genesisConstL
         . key "scriptVersion"
         . _Number

richL :: AsValue t => Traversal' t Scientific
richL = genSpecL
         . key "initializer"
         . key "testnetInitializer"
         . key "testBalance"
         . key "richmen"
         . _Number

svL :: AsValue t => Traversal' t Scientific
svL = key "update"
         . key "applicationVersion"
         . _Number

tagL = key "update"
         . key "systemTag"
         . _String

appNameL :: AsValue t => Traversal' t Text
appNameL = key "update"
         . key "applicationName"
         . _String

bvL = key "update" . key "lastKnownBlockVersion"

bvAltL = bvL . key "bvAlt" . _Number
bvMinorL = bvL . key "bvMinor" . _Number
bvMajorL = bvL . key "bvMajor" . _Number

updateTest :: Y.Value -> Config -> IO ()
updateTest config (Config{..}) = do
    let config' = config & appNameL .~ walletAppName
        config'' = config' & svL %~ (+1)
        showF = (show :: Int -> String) . round
        cFile' = T.pack $ F.encodeString cFile
        (proposeUpdStr, cNodes) = fromMaybe (error "Failed to form update proposal cmd") $ do
            tag <- config ^? tagL
            bvAlt <- config ^? bvAltL
            bvMinor <- config ^? bvMinorL
            bvMajor <- config ^? bvMajorL
            genSlot <- config ^? genSlotL
            genScriptVersion <- config ^? genScriptVersionL
            genBlockSize <- config ^? genBlockSizeL
            sv <- config ^? svL
            (cNodes :: Int) <- round <$> config ^? richL
            return $ (,cNodes) $ "propose-update 0 "
              <> showF bvMajor <> "." <> showF bvMinor <> "." <> showF bvAlt
              <> " " <> showF genScriptVersion <> " " <> showF (genSlot / 1000)
              <> " " <> showF genBlockSize
              <> " " <> T.unpack walletAppName <> ":" <> showF (sv + 1)
              <> " " <> T.unpack tag <> " test_updater.sh none"
    Prelude.putStrLn proposeUpdStr
    Y.encodeFile "run/conf-fresh.yaml" (M.singleton cKey config'')

    TIO.writeFile "test_updater.sh" $ "cp run/conf-fresh.yaml run/cofiguration.wallet.yaml"
    let cmdPropose = L.intercalate "," $
                  (map (\i -> "add-key run/gen-keys/keys-testnet/rich/key"<>show i<>".sk") [1..cNodes])
                  ++ [ proposeUpdStr ]
        keygenBase = "cardano-keygen --configuration-key "<>cKey<>" --configuration-file "<>cFile'<>" --system-start 0"
        auxx cmds = "cardano-auxx --keyfile run/auxx.keys --configuration-key "
            <>cKey<>" --configuration-file "<>cFile'<>" --system-start 0"
            <> " --peer 127.0.0.1:3001 --db-path run/auxx-db cmd --commands \""<>T.pack cmds<>"\""
    shells (keygenBase <> " generate-keys-by-spec --genesis-out-dir run/gen-keys") empty
    shells (keygenBase <> " rearrange --mask 'run/gen-keys/keys-testnet/rich/key*.sk'") empty
    (exitCode, T.lines -> auxxOut) <- shellStrict (auxx cmdPropose) empty
    let fileHash = L.last $ T.words $ head $ filter ("Read file test_updater.sh succesfuly, its hash" `T.isPrefixOf`) auxxOut
    let updHash = L.last $ T.words $ head $ filter ("Update proposal submitted, upId" `T.isPrefixOf`) auxxOut
    TIO.putStrLn $ fileHash <> " " <> updHash
    let cmdVote = L.intercalate "," $
                  (map (\i -> "vote "<>show i<>" y "<>T.unpack updHash) [1..cNodes-1])
    shells (auxx cmdVote) empty

    shells ("mkdir run/serve-upd; mv test_updater.sh run/serve-upd/"<>fileHash) empty



-- buildAMI :: NixOpsConfig -> IO ()
-- buildAMI c = do
--   shells ("nix-build jobsets/deployments.nix -A image -o image -I " <> nixPath c) empty
--   shells "./scripts/create-amis.sh" empty

-- getConfig :: IO (Either String Config)
-- getConfig = do
--   (exitcode, output) <- shellStrict "nix-instantiate --eval --strict --json config.nix" empty
--   return $ eitherDecode (encodeUtf8 $ fromStrict output)
