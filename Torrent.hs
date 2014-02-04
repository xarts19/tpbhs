{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Torrent
( Bytes(..)
, Magnet(..)
, Torrent(..)
, torrentFromTuple
, kilo
, mega
, giga
) where

newtype Bytes = Bytes Integer deriving (Eq, Ord, Num, Read)
newtype Magnet = Magnet String deriving (Eq, Read, Show) 

kilo :: Integer
kilo = 1024

mega :: Integer
mega = 1024 * 1024

giga :: Integer
giga = 1024 * 1024 * 1024

instance Show Bytes where
    show (Bytes b) | b < kilo = show b ++ " B"
                   | b < mega = show kb ++ " KB"
                   | b < giga = show mb ++ " MB"
                   | otherwise = show gb ++ " GB " ++ show (mb `mod` kilo) ++ " MB"
        where
            kb = b `div` kilo
            mb = kb `div` kilo
            gb = mb `div` kilo

data Torrent = Torrent { name :: String, magnet :: Magnet, size :: Bytes, seeders :: Int, leechers :: Int }

instance Show Torrent where
    show Torrent { name = n, magnet = Magnet m, size = sz, seeders = s, leechers = l } =
                    "(" ++ n ++ ", S: " ++ show s ++ ", L: " ++ show l ++ ", Size: " ++ show sz ++ ", " ++ m ++ ")"

torrentFromTuple :: (String, Magnet, Bytes, Int, Int) -> Torrent
torrentFromTuple (n, m, sz, s, l) = Torrent { name = n , magnet = m, size = sz, seeders = s, leechers = l }
