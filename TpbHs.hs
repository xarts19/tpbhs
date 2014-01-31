module TpbHs
( getNewEpisodes
) where

import qualified Network.HTTP as Http
import qualified Network.Browser as Browser
import TpbParser
import UTorrentUtils

newtype URL = URL String deriving (Eq, Read, Show)

type TVShow = String

getPage :: URL -> IO String
getPage (URL url) = do
        (_, rsp) <- Browser.browse $ do
                    Browser.setAllowRedirects True
                    let req = Http.getRequest url
                    let req' = Http.setHeaders req $ removeContentLength $ Http.rqHeaders req
                    Browser.request $ req'
        return (Http.rspBody rsp)
    where
        -- removeContentLength is needed as a workaround for thepiratebay's webserver (lighttpd)
        -- if it sees content-length header with a value "0" in GET request, it returns error 400
        removeContentLength :: [Http.Header] -> [Http.Header]
        removeContentLength [] = []
        removeContentLength ((Http.Header Http.HdrContentLength _) : hdrs) = removeContentLength hdrs
        removeContentLength (hdr : hdrs) = hdr : removeContentLength hdrs



--deduceTVShow :: 



--startTorrentDownload :: TVShow -> Torrent -> 


getNewEpisodes :: IO ()
getNewEpisodes = do
    page <- getPage $ URL "http://thepiratebay.se/search/mentalist/0/7/208"
    putStrLn ""
    case parseResultsPage page of
         Left err -> putStrLn err
         Right rs -> mapM_ putStrLn (map show rs)
    
    ret <- startMagnetDonwload "D:\\Internet\\New folder (4)" $ Magnet "magnet:?xt=urn:btih:38135cb873317dea857c8737a3c860d3139a9448&dn=The.Mentalist.S06E12.720p.HDTV.X264-DIMENSION%5Brartv%5D&tr=udp%3A%2F%2Ftracker.openbittorrent.com%3A80&tr=udp%3A%2F%2Ftracker.publicbt.com%3A80&tr=udp%3A%2F%2Ftracker.istole.it%3A6969&tr=udp%3A%2F%2Ftracker.ccc.de%3A80&tr=udp%3A%2F%2Fopen.demonii.com%3A1337"
    
    putStrLn $ show ret
