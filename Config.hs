{-# LANGUAGE FlexibleContexts #-}

module Config
( Config
, parseConfig
, writeConfig
) where

import Data.Maybe
import Data.ConfigFile
import Control.Monad
import TVShow

type Config = [TVShow]

parseConfig :: String -> Either String Config
parseConfig x = case parseConfig' x of
                     Right c -> Right c
                     Left (cp_error, loc) -> Left $ (show cp_error) ++ " in " ++ loc

parseConfig' :: String -> Either CPError Config
parseConfig' config_content = do
    cp <- readstring emptyCP config_content
    return $ map (parseShow cp) (sections cp)

parseShow :: ConfigParser -> SectionSpec -> TVShow
parseShow cp tvshow = TVShow tvshow folder ld
    where
        ld = getEpisode (get cp tvshow "last_downloaded")
        getEpisode = either (\_ -> firstEpisode) (\s -> fromMaybe firstEpisode (parseEpisode s))
        folder = getFolder (get cp tvshow "folder")
        getFolder = either (\_ -> tvshow) (\s -> s)

writeConfig :: Config -> Either String String
writeConfig config = case foldM addShow emptyCP config of
                        Left err -> Left $ show err
                        Right cfg -> Right $ to_string cfg
    where
        addShow cp (TVShow name folder ep) = do
            let sect = name
            let x = cp
            x <- add_section x sect
            x <- set x sect "folder" folder
            x <- setshow x sect "last_downloaded" ep
            return x
