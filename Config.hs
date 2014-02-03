{-# LANGUAGE FlexibleContexts #-}

module Config
( TVShowConfig(..)
, Config
, parseConfig
) where

import Data.ConfigFile
import Torrent (Episode, firstEpisode)

data TVShowConfig = TVShowConfig { search_term :: String
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
    let tvshows = sections cp
    return $ map (parseShow cp base_dir) tvshows
    where
        parseShow :: ConfigParser -> String -> SectionSpec -> TVShowConfig
        parseShow cp base_dir tvshow = 
            let st' = do get cp tvshow "search_term"
                st = case st' of
                        Right t -> t
                        Left _ -> tvshow
            in TVShowConfig { search_term =  st
                            , folder = base_dir ++ tvshow
                            , last_watched = firstEpisode
                            , last_downloaded = firstEpisode }
