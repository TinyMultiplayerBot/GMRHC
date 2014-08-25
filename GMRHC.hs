{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Maybe (fromMaybe)
import Network.Http.Client
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as C

import JSON

apiURL :: String
apiURL = "http://multiplayerrobot.com/api/Diplomacy/"

authPlayer :: String -> IO C.ByteString
authPlayer ak = get (C.pack (apiURL ++"AuthenticateUser?authKey=" ++ ak)) concatHandler

getGamesAndPlayers :: String -> String -> IO C.ByteString
getGamesAndPlayers ak pid = get (C.pack (apiURL ++ "GetGamesAndPlayers?playerIDText=" ++ pid ++ "&authKey=" ++ ak)) concatHandler

decodeGAndP :: Maybe GamesAndPlayers -> GamesAndPlayers
decodeGAndP = fromMaybe emptyGAP

genDLurl :: String -> String -> C.ByteString
genDLurl ak gid = C.pack $ apiURL ++ "GetLatestSaveFileBytes?authkey=" ++ ak ++ "&gameId=" ++ gid

downloadSave _ [] = return ()
downloadSave ak (g:gList) = do
    Prelude.putStrLn $ "Downloading " ++ name g
    save <- get (genDLurl ak (show $ gameId g)) concatHandler
    C.writeFile (name g ++ ".civ5save") save
    downloadSave ak gList

getLatestSaveFileByets :: String -> IO ()
getLatestSaveFileByets authKey = do
    playerID <- authPlayer authKey
    result <- getGamesAndPlayers authKey (C.unpack playerID)
    let decoded = decodeGAndP (decodeStrict result :: Maybe GamesAndPlayers)
    downloadSave authKey (games decoded)
    Prelude.putStrLn "Done"

helpStr = unwords ["GMRHC --key <API KEY> [--games] [--upload <game id> <save path>]\n\n",
                   "Giant Multiplayer Robot Haskell Client\n",
                   "A Simple file upload and download CLI tool for Giant Multiplayer Robot\n\n",
                   "    --key                     - Your API Key. Using this alone downloads all your saves\n"]

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["--key", authKey] -> getLatestSaveFileByets authKey
        _ -> Prelude.putStrLn helpStr
