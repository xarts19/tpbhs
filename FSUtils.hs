module FSUtils
( updateLastDownloaded
, pathValid
) where

import Data.Maybe
import System.Directory
import qualified TVShow as TV

import Debug.Trace

updateLastDownloaded :: [TV.TVShow] -> IO [TV.TVShow]
updateLastDownloaded config = mapM update' config
    where
        update' tvshow = do
            let dir = TV.folder tvshow
            exists <- doesDirectoryExist dir
            if (exists) then do
                            contents <- getDirectoryContents dir
                            let episodes = catMaybes $ map (TV.parseEpisodeFromFilename tvshow) contents
                            let lastEp = maximum $ (TV.last_downloaded tvshow):episodes
                            if (lastEp > TV.last_downloaded tvshow)
                               then do
                                   putStrLn $ "Updating last watched for '" ++ TV.name tvshow ++ "' from " ++
                                              show (TV.last_downloaded tvshow) ++ " to " ++ show lastEp
                                   return tvshow { TV.last_downloaded = lastEp }
                               else return tvshow
                        else do
                            traceIO $ "Directory not found: " ++ show dir
                            return tvshow

pathValid :: TV.TVShow -> IO Bool
pathValid tvshow  = doesDirectoryExist (TV.folder tvshow)
