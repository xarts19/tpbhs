{-# LANGUAGE FlexibleContexts #-}

module Config
( Episode
, TVShowConfig(..)
, Config
, parseConfig
) where

import Data.Char
import Data.ConfigFile
import Text.Printf
import Control.Monad
import Control.Monad.Error

data Episode = Episode Int Int

instance Show Episode where
    show (Episode s e) = printf "S%02d%02d" s e

parseEpisode :: String -> Maybe Episode
parseEpisode str = parseEpisode' $ map toLower $ str
    where
        parseEpisode' ('s':s1:s2:'e':e1:e2:[]) = Just $ Episode (read [s1, s2]) (read [e1, e2])
        parseEpisode' _ = Nothing

data TVShowConfig = TVShowConfig { search_term :: String
                                 , folder :: String
                                 , last_watched :: Episode
                                 , last_downloaded :: Episode } deriving (Show)

type Config = [TVShowConfig]

parseConfig config_content = do
    cp <- readstring emptyCP config_content
    base_dir <- get cp "DEFAULT" "base_folder"
    tvshows <- sections cp
    return tvshows
    
{--
parseConfig :: String -> Either String Config
parseConfig config_content = liftM sections cp >>= map parseShow
    where
        cp = readstring emptyCP config_content
        base_dir = liftM get cp "DEFAULT" "base_folder"
        
        parseShow :: MonadError CPError m => SectionSpec -> m TVShowConfig
        parseShow tvshow = runErrorT $ do
            parser <- cp
            let has_search_term = has_option parser tvshow "search_term"
            st <- if (has_search_term) then get parser tvshow "search_term"
                                            else return tvshow
            return TVShowConfig { search_term =  st, folder = base_dir ++ tvshow, last_watched = Episode 1 1, last_downloaded = Episode 1 1 }
            --}
            