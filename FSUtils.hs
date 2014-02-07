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
            absolute_path <- canonicalizePath filename
            let ep = foldl1 (<|>) $ Nothing : (map lastEpisodeFile files)
            let eps = catMaybes $ map parseEpisode $ filter (isTitleValid show_name) files
            
            -- print some useful information
            case ep of
                Nothing -> return ()
                Just e -> (putStrLn $ "Processing '" ++ filename ++ "'...") >>
                          (putStrLn $ "Latest watched episode: " ++ show e)
            case eps of
                [] -> return ()
                xs -> putStrLn $ "Latest episode on disk: " ++ show (maximum xs)
            
            -- if some episodes are present that are more recent then last episode file suggests
            -- then use them
            return $ fmap (\e -> TVShow show_name absolute_path e) $ fmap (\e -> maximum (e:eps)) ep

enumShows :: IO [TVShow]
enumShows = do
    cwd <- getCurrentDirectory
    folders <- getDirectoryContents cwd
    liftM (catMaybes) $ mapM getTVShow folders
