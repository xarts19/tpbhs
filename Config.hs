{-# LANGUAGE FlexibleContexts #-}

module Config
( Config
, tvshows
, parseConfig
, writeConfig
) where

import Data.Char
import Data.Maybe
import Data.ConfigFile
import Control.Monad
import TVShow

data Config = Config { tvshows :: [TVShow], base_directory :: String }

parseConfig :: String -> Either String Config
parseConfig x = case parseConfig' x of
                     Right c -> Right c
                     Left (cp_error, loc) -> Left $ (show cp_error) ++ " in " ++ loc

parseConfig' :: String -> Either CPError Config
parseConfig' config_content = do
    cp <- readstring emptyCP config_content
    base_dir <- get cp "DEFAULT" "base_folder"
    return $ Config { base_directory = base_dir, tvshows = map (parseShow cp base_dir) (sections cp) }

parseShow :: ConfigParser -> String -> SectionSpec -> TVShow
parseShow cp base_dir tvshow = 
    let st = either (\_ -> tvshow) id (get cp tvshow "search_term")
        lw = getEpisode (get cp tvshow "last_watched")
        ld = getEpisode (get cp tvshow "last_downloaded")
    in TVShow { name = tvshow
              , search_term = map toLower st
              , folder = base_dir ++ tvshow
              , last_watched = lw
              , last_downloaded = ld }
    where
        getEpisode = either (\_ -> firstEpisode) (\s -> fromMaybe firstEpisode (parseEpisode s)) 

writeConfig :: Config -> Either String String
writeConfig config = case baseCP >>= \x -> foldM addShow x $ tvshows config of
                        Left err -> Left $ show err
                        Right cfg -> Right $ to_string cfg
    where
        baseCP = set emptyCP "DEFAULT" "base_folder" $ base_directory config
        addShow cp tvshow = do
            let sect = name tvshow
            let x = cp
            x <- add_section x sect
            x <- if (search_term tvshow /= name tvshow) then set x sect "search_term" (search_term tvshow)
                                                        else return x
            x <- setshow x sect "last_downloaded" (last_downloaded tvshow)
            x <- setshow x sect "last_watched" (last_watched tvshow)
            return x
