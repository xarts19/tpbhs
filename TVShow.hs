module TVShow
( TVShow(..)
, Episode
, parseEpisode
, firstEpisode
, nextEpisode
, nextSeason
, lastFromEpisodes
, parseEpisodeFromTitle
) where

import Data.Char
import Text.Printf
import Text.Regex.Posix
import Text.Read
import Control.Applicative

--import Debug.Trace

data Episode = Episode Int Int deriving (Eq)

instance Show Episode where
    show (Episode s e) = printf "S%02dE%02d" s e

instance Ord Episode where
    compare (Episode s1 e1) (Episode s2 e2) | s1 < s2 = LT
                                            | s1 > s2 = GT
                                            | s1 == s2 && e1 < e2 = LT
                                            | s1 == s2 && e1 > e2 = GT
                                            | otherwise = EQ

{--
let m = match' $ map toLower $ str
in trace (str ++ " -> " ++ show m) parse' m
--}
parseEpisode :: String -> Maybe Episode
parseEpisode str = parse' $ match' $ map toLower $ str
    where
        match' s = getAllTextSubmatches (s =~ "s([0-9]?[0-9])e?([0-9]?[0-9])" :: AllTextSubmatches [] String)
        parse' (_:s:ep:[]) = Episode <$> readMaybe s <*> readMaybe ep
        parse' _ = Nothing

firstEpisode :: Episode
firstEpisode = Episode 1 1

nextEpisode :: Episode -> Episode
nextEpisode (Episode s e) = Episode s (e+1)

nextSeason :: Episode -> Episode
nextSeason (Episode s _) = Episode (s+1) 1

lastFromEpisodes :: [Episode] -> Episode
lastFromEpisodes = maximum


data TVShow = TVShow String FilePath Episode deriving (Show)

myWords :: String -> [String]
myWords s = case dropWhile predicate s of
                      "" -> []
                      s' -> w : myWords s''
                            where (w, s'') = break predicate s'
    where
        predicate = not . isAlphaNum

type TVShowName = String

parseEpisodeFromTitle :: TVShowName -> String -> Maybe Episode
parseEpisodeFromTitle show_name filename =
    let does_match = all (\x -> (map toLower filename =~ x) :: Bool) $ myWords show_name
    in if (does_match) then parseEpisode filename
                       else Nothing
