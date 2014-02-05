module TpbHs
( getNewEpisodes
) where

import Data.Ord
import Data.List
import Data.Maybe
import Control.Monad
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

getTorrentFor :: TVShow -> Episode -> Quality -> IO (Maybe T.Torrent)
getTorrentFor tvshow episode q = do
    let query = search_term tvshow
    let search = query ++ " " ++ show episode
    putStrLn $ "Searching for '" ++ search ++ "'..."
    page <- getPage $ tpb_get_search_url q search
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

runTorrent :: TVShow -> T.Torrent -> IO Bool
runTorrent tvshow torrent = do
    ret <- startMagnetDonwload (folder tvshow) (T.magnet torrent)
    case ret of
         Nothing -> do
             putStrLn $ "Torrent '" ++ T.name torrent ++ "' started"
             return True
         Just code -> do
             putStrLn $ "Failed to start torrent '" ++ T.name torrent ++ "': Code " ++ show code
             return False

findFirstJust :: [IO (Maybe T.Torrent)] -> IO (Maybe T.Torrent)
findFirstJust [] = return Nothing
findFirstJust (x:xs) = do
    res <- x
    case res of
        Nothing -> findFirstJust xs
        Just val -> return $ Just val

getNewEpisodes'' :: TVShow -> IO TVShow
getNewEpisodes'' tvshow = do
    valid <- pathValid tvshow
    case valid of
         True -> getNewEpisodes' tvshow
         False -> do
             putStrLn $ "No folder for '" ++ name tvshow ++ "'. Aborting"
             return tvshow

--startTorrentDownload :: TVShow -> Torrent -> 
getNewEpisodes' :: TVShow -> IO TVShow
getNewEpisodes' tvshow = do
    let ld = last_downloaded tvshow
    let nextEp = nextEpisode ld
    let nextSe = nextSeason ld
    tor <- findFirstJust [getTorrentFor tvshow nextEp HD, getTorrentFor tvshow nextEp SD, 
                          getTorrentFor tvshow nextSe HD, getTorrentFor tvshow nextSe SD]
    case tor of
         Nothing -> do
             putStrLn "Giving up"
             return tvshow
         Just t -> do
             res <- runTorrent tvshow t
             case res of
                  True -> do
                      let newEp = fromMaybe ld (parseEpisode $ T.name t)
                      getNewEpisodes' $ tvshow { last_downloaded = newEp }
                  False -> do
                      putStrLn "Aborting"
                      return tvshow

getNewEpisodes :: IO ()
getNewEpisodes = do
    config <- liftM parseConfig $ readFile configFilename
    case config of
         Left err -> putStrLn err
         Right cfg -> do
             tvshows' <- updateLastDownloaded $ tvshows cfg
             tvshows'' <- mapM getNewEpisodes'' $ tvshows'
             let cfg' = cfg { tvshows = tvshows'' }
             case writeConfig cfg' of
                  Left err -> putStrLn err
                  Right str -> writeFile configFilename str

{--
    page <- getPage $ "http://thepiratebay.se/search/mentalist/0/7/208"
    putStrLn ""
    case parseResultsPage page of
         Left err -> putStrLn err
         Right rs -> mapM_ putStrLn (map show rs)
    
    ret <- startMagnetDonwload "D:\\Internet\\New folder (4)" $ Magnet "magnet:?xt=urn:btih:38135cb873317dea857c8737a3c860d3139a9448&dn=The.Mentalist.S06E12.720p.HDTV.X264-DIMENSION%5Brartv%5D&tr=udp%3A%2F%2Ftracker.openbittorrent.com%3A80&tr=udp%3A%2F%2Ftracker.publicbt.com%3A80&tr=udp%3A%2F%2Ftracker.istole.it%3A6969&tr=udp%3A%2F%2Ftracker.ccc.de%3A80&tr=udp%3A%2F%2Fopen.demonii.com%3A1337"
    
    putStrLn $ show ret
    --}
