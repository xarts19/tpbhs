{-# LANGUAGE FlexibleContexts #-}

module Config
( TVShowConfig(..)
, Config
, parseConfig
) where

import Data.Maybe
import Data.ConfigFile
import Torrent (Episode, firstEpisode, parseEpisode)

data TVShowConfig = TVShowConfig { name :: String
                                 , search_term :: String
                                 , folder :: String
                                 , last_watched :: Episode
                                 , last_downloaded :: Episode } deriving (Show)

type Config = [TVShowConfig]

parseConfig :: String -> Either String Config
parseConfig x = case parseConfig' x of
                     Right c -> Right c
                     Left (cp_error, loc) -> Left $ (show cp_error) ++ " in " ++ loc

parseConfig' :: String -> Either CPError Config
parseConfig' config_content = do
    cp <- readstring emptyCP config_content
    base_dir <- get cp "DEFAULT" "base_folder"
    return $ map (parseShow cp base_dir) (sections cp)

parseShow :: ConfigParser -> String -> SectionSpec -> TVShowConfig
parseShow cp base_dir tvshow = 
    let st = either (\_ -> tvshow) id (get cp tvshow "search_term")
        lw = getEpisode (get cp tvshow "last_watched")
        ld = getEpisode (get cp tvshow "last_downloaded")
    in TVShowConfig { name = tvshow
                    , search_term = st
                    , folder = base_dir ++ tvshow
                    , last_watched = lw
                    , last_downloaded = ld }
    where
        getEpisode = either (\_ -> firstEpisode) (\s -> fromMaybe firstEpisode (parseEpisode s)) 