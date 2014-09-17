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
    C.writeFile (name g ++ ".Civ5Save") save
    downloadSave ak gList

getLatestSaveFileByets :: String -> IO ()
getLatestSaveFileByets authKey = do
    playerID <- authPlayer authKey
    result <- getGamesAndPlayers authKey (C.unpack playerID)
    let decoded = decodeGAndP (decodeStrict result :: Maybe GamesAndPlayers)
    downloadSave authKey (games decoded)
    Prelude.putStrLn "Done"

listGamesHelper [] = return ()
listGamesHelper (g:gs) = do
    Prelude.putStrLn ((name g) ++ " - " ++ (show $ turnId $ currentTurn g))
    listGamesHelper gs

listGames :: String -> IO ()
listGames ak = do
    playerID <- authPlayer ak
    result <- getGamesAndPlayers ak (C.unpack playerID)
    let decoded = decodeGAndP ( (decodeStrict result) :: Maybe GamesAndPlayers)
    let gs = games decoded
    listGamesHelper gs

genULurl :: String -> String -> String
genULurl ak tId = apiURL ++ "SubmitTurn?authKey=" ++ ak ++ "&turnId=" ++ tId

submitTurn :: String -> String -> FilePath -> IO ()
submitTurn ak tId fp = do
    save <- C.readFile fp
    res <- post (C.pack (genULurl ak tId)) "application/civ5Save"(fileBody fp) concatHandler
    Prelude.putStrLn "This output is currently not well understood, thus raw output:"
    C.putStrLn res


helpStr = unwords ["GMRHC [download | games | upload <turn id> <save file>]\n\n",
                   "Giant Multiplayer Robot Haskell Client\n",
                   "A Simple file upload and download CLI tool for Giant Multiplayer Robot\n\n",
                   "    download                  - Download your current game saves\n",
                   "    games                     - List all games and Turn IDs. Needed for uploading\n",
                   "    upload <turn id> <save>   - Uploads the save file"]

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["downloa"]        -> getLatestSaveFileByets authKey
        ["games"]          -> listGames authKey
        ["upload", iD, fp] -> submitTurn authKey iD fp
        _ -> Prelude.putStrLn helpStr
