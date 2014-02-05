module TpbHs
( getNewEpisodes
) where

import Data.Ord
import Data.List
import Data.Maybe
import Control.Applicative
import Network.HTTP
import qualified Torrent as T
import TpbParser
import UTorrentUtils
import Config
import FSUtils
import TVShow

configFilename :: String
configFilename = "tpbhs.config"

getPage :: String -> IO String
getPage url = do
        let req = getRequest url
        let req' = setHeaders req $ filter (not . isContentLength) $ rqHeaders req
        response <- simpleHTTP req'
        getResponseBody response
    where
        -- 'filter (not . isContentLength)' is needed as a workaround for thepiratebay's webserver (lighttpd)
        -- if it sees content-length header with a value "0" in GET request, it returns error 400
        isContentLength (Header HdrContentLength _) = True
        isContentLength (Header _ _) = False


data Quality = HD | SD

tpb_section_tvshows :: Quality -> String
tpb_section_tvshows HD = "208"
tpb_section_tvshows SD = "205"

tpb_sortby_seeders :: String
tpb_sortby_seeders = "7"

tpb_get_search_url :: Quality -> String -> String
tpb_get_search_url q search = "http://thepiratebay.se/search/" ++ urlEncode search ++
                              "/0/" ++ tpb_sortby_seeders ++ "/" ++ tpb_section_tvshows q

getTorrentFor :: String -> Episode -> Quality -> IO (Maybe T.Torrent)
getTorrentFor tvshow_name episode q = do
    let query = tvshow_name
    let search = query ++ " " ++ show episode
    let search' = case Quality of
                    HD -> search ++ " 720p"
                    SD -> search
    putStrLn $ "Searching for '" ++ search' ++ "'..."
    page <- getPage $ tpb_get_search_url q search'
    case parseResultsPage page of
         Left err -> do
             putStrLn $ "Nothing found: " ++ err
             return Nothing
         Right table -> do
             let filtered = filter (liftA2 (&&) sizeCorrect hasSeeds) table
             let sorted = reverse $ sortBy (comparing T.seeders) filtered
             if (null sorted) then do
                                putStrLn "Nothing found"
                                return Nothing
                              else
                                return $ Just $ head sorted
    where
        sizeCorrect tor = T.size tor > T.Bytes (100 * T.mega) && T.size tor < T.Bytes (3 * T.giga)
        hasSeeds tor = T.seeders tor > 0

runTorrent :: FilePath -> T.Torrent -> IO Bool
runTorrent folder torrent = do
    ret <- startMagnetDonwload folder (T.magnet torrent)
    case ret of
         Nothing -> do
             putStrLn $ "Torrent '" ++ T.name torrent ++ "' started"
             return True
         Just code -> do
             putStrLn $ "Failed to start torrent '" ++ T.name torrent ++ "': Code " ++ show code
             return False

findFirstValid :: [IO (Maybe T.Torrent)] -> IO (Maybe T.Torrent)
findFirstValid [] = return Nothing
findFirstValid (x:xs) = do
    res <- x
    case res of
        Nothing -> findFirstValid xs
        Just val -> return $ Just val

getNewEpisodes' :: TVShow -> IO ()
getNewEpisodes' (TVShow name folder ep) = do
    let nextEp = nextEpisode ep
    let nextSe = nextSeason ep
    tor <- findFirstValid [getTorrentFor name nextEp HD, getTorrentFor name nextEp SD, 
                           getTorrentFor name nextSe HD, getTorrentFor name nextSe SD]
    case tor of
         Nothing -> putStrLn "Giving up"
         Just t -> do
             res <- runTorrent folder t
             case res of
                  True -> do
                      let newEp = fromMaybe ep (parseEpisode $ T.name t)
                      getNewEpisodes' $ TVShow name folder newEp
                  False -> do
                      putStrLn "Aborting"

getNewEpisodes :: IO ()
getNewEpisodes = do
    tvshows <- enumShows
    _ <- mapM getNewEpisodes' tvshows
    case writeConfig tvshows of
      Left err -> putStrLn err
      Right str -> writeFile configFilename str
