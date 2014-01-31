{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Torrent
( Bytes(..)
, Magnet(..)
, Torrent(..)
, torrentFromTuple
) where

newtype Bytes = Bytes Integer deriving (Eq, Ord, Num, Read, Show)
newtype Magnet = Magnet String deriving (Eq, Read, Show) 

data Torrent = Torrent { name :: String, magnet :: Magnet, size :: Bytes, seeders :: Int, leechers :: Int }

instance Show Torrent where
    show Torrent { name = n, magnet = Magnet m, size = Bytes sz, seeders = s, leechers = l } =
                    "(" ++ n ++ ", S: " ++ show s ++ ", L: " ++ show l ++ ", Size: " ++ show sz ++ ", " ++ m ++ ")"

torrentFromTuple :: (String, Magnet, Bytes, Int, Int) -> Torrent
torrentFromTuple (n, m, sz, s, l) = Torrent { name = n , magnet = m, size = sz, seeders = s, leechers = l }

