module FSUtils
( enumShows
) where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import System.Directory
import System.FilePath
import TVShow

lastEpisodeFile :: FilePath -> Maybe Episode
lastEpisodeFile filename = 
    if isPrefixOf "last-" filename
        then parseEpisode filename
        else Nothing

getTVShow :: FilePath -> IO (Maybe TVShow)
getTVShow filename = do
    let show_name = takeBaseName filename
    exists <- doesDirectoryExist filename
    case exists of
        False -> return Nothing
        True -> do
            files <- getDirectoryContents filename
            let ep = foldl1 (<|>) $ Nothing : (map lastEpisodeFile files)
            let eps = catMaybes $ map (parseEpisodeFromTitle show_name) files
            
            -- if some episodes are present that are more recent then last episode file suggests
            -- then use them
            return $ fmap (\e -> TVShow show_name filename e) $ fmap (\e -> maximum (e:eps)) ep

enumShows :: IO [TVShow]
enumShows = do
    cwd <- getCurrentDirectory
    folders <- getDirectoryContents cwd
    liftM (catMaybes) $ mapM getTVShow folders
