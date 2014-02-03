{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Torrent
( Bytes(..)
, Magnet(..)
, Torrent(..)
, Episode
, torrentFromTuple
, parseEpisode
, firstEpisode
, nextEpisode
, nextSeason
) where

import Data.Char
import Text.Printf

newtype Bytes = Bytes Integer deriving (Eq, Ord, Num, Read, Show)
newtype Magnet = Magnet String deriving (Eq, Read, Show) 

data Torrent = Torrent { name :: String, magnet :: Magnet, size :: Bytes, seeders :: Int, leechers :: Int }

instance Show Torrent where
    show Torrent { name = n, magnet = Magnet m, size = Bytes sz, seeders = s, leechers = l } =
                    "(" ++ n ++ ", S: " ++ show s ++ ", L: " ++ show l ++ ", Size: " ++ show sz ++ ", " ++ m ++ ")"

torrentFromTuple :: (String, Magnet, Bytes, Int, Int) -> Torrent
torrentFromTuple (n, m, sz, s, l) = Torrent { name = n , magnet = m, size = sz, seeders = s, leechers = l }

data Episode = Episode Int Int

instance Show Episode where
    show (Episode s e) = printf "S%02d%02d" s e

parseEpisode :: String -> Maybe Episode
parseEpisode str = parseEpisode' $ map toLower $ str
    where
        parseEpisode' ('s':s1:s2:'e':e1:e2:[]) = Just $ Episode (read [s1, s2]) (read [e1, e2])
        parseEpisode' _ = Nothing

firstEpisode :: Episode
firstEpisode = Episode 1 1

nextEpisode :: Episode -> Episode
nextEpisode (Episode s e) = Episode s (e+1)

nextSeason :: Episode -> Episode
nextSeason (Episode s _) = Episode (s+1) 1
