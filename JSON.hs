{-# LANGUAGE OverloadedStrings #-}

module JSON where

import Control.Applicative
import Control.Monad
import Data.Aeson

emptyGAP = GamesAndPlayers [] [] (-1)

data GamesAndPlayers = GamesAndPlayers
    { games              :: [Game]
    , players            :: [Player]
    , currentTotalPoints :: Int
    } deriving Show

instance FromJSON GamesAndPlayers where
    parseJSON (Object o) = GamesAndPlayers <$>
                           o .: "Games" <*>
                           o .: "Players" <*>
                           o .: "CurrentTotalPoints"
    parseJSON _          = mzero

data Game = Game
    { name        :: String
    , gameId      :: Int
    , gPlayers    :: [GPlayer]
    , currentTurn :: CurrentTurn
    , gType       :: Maybe Int
    } deriving Show

instance FromJSON Game where
    parseJSON (Object o) = Game <$>
                           o .: "Name" <*>
                           o .: "GameId" <*>
                           o .: "Players" <*>
                           o .: "CurrentTurn" <*>
                           o .: "Type"
    parseJSON _          = mzero

data GPlayer = GPlayer
    { userId    :: Int
    , turnOrder :: Int
    } deriving Show

instance FromJSON GPlayer where
    parseJSON (Object o) = GPlayer <$>
                           o .: "UserId" <*>
                           o .: "TurnOrder"
    parseJSON _          = mzero

data Player = Player
    { steamID      :: Int
    , personaName  :: String
    , avatarUrl    :: String
    , personaState :: Int
    , gameID       :: Int
    } deriving Show

instance FromJSON Player where
    parseJSON (Object o) = Player <$>
                           o .: "SteamID" <*>
                           o .: "PersonaName" <*>
                           o .: "AvatarUrl" <*>
                           o .: "PersonaState" <*>
                           o .: "GameID"
    parseJSON _          = mzero

data CurrentTurn = CurrentTurn
    { turnId       :: Int
    , number       :: Int
    , ctuserId     :: Int
    , started      :: String
    , expires      :: Maybe String
    , playerNumber :: Int
    , isFirstTurn  :: Bool
    } deriving Show

instance FromJSON CurrentTurn where
    parseJSON (Object o) = CurrentTurn <$>
                           o .: "TurnId" <*>
                           o .: "Number" <*>
                           o .: "UserId" <*>
                           o .: "Started" <*>
                           o .: "Expires" <*>
                           o .: "PlayerNumber" <*>
                           o .: "IsFirstTurn"
    parseJSON _          = mzero
