module TVShow
( TVShow(..)
, Episode
, parseEpisode
, firstEpisode
, nextEpisode
, nextSeason
, lastFromEpisodes
, isTitleValid
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
                                            
episodeRegex :: String
episodeRegex = "s([0-9]?[0-9])e?([0-9]?[0-9])"

separatorRegex :: String
separatorRegex = "([._ -]|the|a)*"

yearRegex :: String
yearRegex = "([1-2][0-9][0-9][0-9])?"

rMatch :: String -> String -> [String]
rMatch str regex = getAllTextSubmatches (str =~ regex :: AllTextSubmatches [] String)

joinRegex :: [String] -> String
joinRegex [] = []
joinRegex (x:[]) = x
joinRegex (x:xs) = x ++ separatorRegex ++ joinRegex xs

parseEpisode :: String -> Maybe Episode
parseEpisode str = parse' $ match' $ map toLower $ str
    where
        match' s = rMatch s episodeRegex
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

isTitleValid :: TVShowName -> String -> Bool
isTitleValid showName title = (not . null) $ rMatch normTitle nameAndEpRegex
    where
        normTitle = map toLower title
        normShowName = map toLower showName
        nameAndEpRegex = joinRegex (["^"] ++ myWords normShowName ++ [yearRegex, episodeRegex])
