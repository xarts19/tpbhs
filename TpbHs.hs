{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TbpHs where

import Control.Monad
import Data.Maybe
import Data.List
import System.Process
import System.Exit

import Text.HTML.TagSoup

import qualified Network.HTTP as Http
import qualified Network.Browser as Browser

newtype URL = URL String deriving (Eq, Read, Show)
newtype Bytes = Bytes Int deriving (Eq, Ord, Num, Read, Show)
newtype Magnet = Magnet String deriving (Eq, Read, Show)

type Error = String
type Page = String
type Filepath = String
type TVShow = String


data Torrent = Torrent { torrent_name :: String, magnet :: Magnet, size :: Bytes, seeders :: Int, leechers :: Int }

instance Show Torrent where
    show Torrent { torrent_name = n, magnet = Magnet m, size = Bytes sz, seeders = s, leechers = l } =
                    "(" ++ n ++ ", S: " ++ show s ++ ", L: " ++ show l ++ ", Size: " ++ show sz ++ ", " ++ m ++ ")"

torrentFromTuple :: (String, Magnet, Bytes, Int, Int) -> Torrent
torrentFromTuple (n, m, sz, s, l) = Torrent { torrent_name = n , magnet = m, size = sz, seeders = s, leechers = l }



removeContentLength :: [Http.Header] -> [Http.Header]
removeContentLength [] = []
removeContentLength ((Http.Header Http.HdrContentLength _) : hdrs) = removeContentLength hdrs
removeContentLength (hdr : hdrs) = hdr : removeContentLength hdrs

getPage :: URL -> IO String
getPage (URL url) = do
    (_, rsp) <- Browser.browse $ do
                  Browser.setAllowRedirects True
                  let req = Http.getRequest url
                  let req' = Http.setHeaders req $ removeContentLength $ Http.rqHeaders req
                  Browser.request $ req'
    return (Http.rspBody rsp)



extract :: String -> [(String, String)] -> [Tag String] -> [[Tag String]]
extract tag attributes tags = map (takeWhile (/= TagClose tag)) $ sections (~== TagOpen tag attributes) tags

extractTag :: String -> [Tag String] -> [[Tag String]]
extractTag tag tags = extract tag [] tags

findTagText :: [Tag String] -> Maybe String
findTagText tags = fmap fromTagText $ find isTagText tags

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x



parseTable :: String -> Either Error [Tag String]
parseTable page = case maybeHead $ extract "table" [("id", "searchResult")] $ parseTags page of
                       Nothing -> Left "No table found"
                       Just t -> Right t

parseRows :: [Tag String] -> [[Tag String]]
parseRows table = extractTag "tr" table

parseColumns :: [Tag String] -> [[Tag String]]
parseColumns singleRow = extractTag "td" singleRow

magnetStr :: String
magnetStr = "Download this torrent using magnet"

decodeName :: [Tag String] -> Maybe String
decodeName descr_tags = needed_tag >>= findTagText
    where
        needed_tag = maybeHead $ extract "a" [("class", "detLink")] descr_tags

decodeMagnet :: [Tag String] -> Maybe Magnet
decodeMagnet descr_tags = fmap Magnet $ fmap (fromAttrib "href") $ needed_tag >>= maybeHead
    where
        needed_tag = maybeHead $ extract "a" [("title", "Download this torrent using magnet")] descr_tags

findSize :: String -> Maybe String
findSize descr = case afterSize of { "" -> Nothing; str -> Just str }
    where
        afterSize = (takeWhile (/= ',') . unwords . drop 1 . dropWhile (/= "Size") . words) descr

parseSize :: String -> Maybe Bytes
parseSize size_str = fmap Bytes $ fmap round $ parse $ words size_str
    where
        parse (n:mult:[]) = Just $ (read n :: Double) * getMult mult
        parse _ = Nothing
        getMult "GiB" = 10 ^ 9
        getMult "MiB" = 10 ^ 6
        getMult "KiB" = 10 ^ 3
        getMult "B" = 1

decodeSize :: [Tag String] -> Maybe Bytes
decodeSize descr_tags = needed_tag >>= findTagText >>= findSize >>= parseSize
    where
        needed_tag = maybeHead $ extract "font" [("class", "detDesc")] descr_tags

decodeInt :: [Tag String] -> Maybe Int
decodeInt int_tags = fmap read $ findTagText int_tags

maybeTuple :: (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e) -> Maybe (a, b, c, d, e)
maybeTuple (Just a, Just b, Just c, Just d, Just e) = Just (a, b, c, d, e)
maybeTuple _ = Nothing

decodeRow :: [[Tag String]] -> Maybe Torrent
decodeRow (_:descr:s:l:[]) = fmap torrentFromTuple $ maybeTuple (decodeName descr, decodeMagnet descr, decodeSize descr, decodeInt s, decodeInt l)
decodeRow _ = Nothing



namesAndMagnets :: Page -> Either Error [Torrent]
namesAndMagnets page = liftM catMaybes $ liftM (map decodeRow) columns
    where
        columns = liftM (map parseColumns) $ liftM parseRows $ parseTable page


deduceTVShow :: 


utorrentCommand :: String
utorrentCommand = "\"C:\\Program Files (x86)\\uTorrent\\uTorrent.exe\" /DIRECTORY"

startMagnetDonwload :: Filepath -> Magnet -> IO (Maybe Int)
startTorrentDonwload destination_folder (Magnet magnet) = do
    (_, _, _, pHandle) <- createProcess $ shell command
    ret <- waitForProcess pHandle
    case ret of
         ExitSuccess -> return Nothing
         ExitFailure code -> return $ Just code
    where
        command = utorrentCommand ++ " \"" ++ destination_folder ++ "\" \"" ++ magnet ++ "\""

startTorrentDownload :: TVShow -> Torrent -> 


main :: IO ()
main = do
    page <- getPage $ URL "http://thepiratebay.se/search/mentalist/0/7/208"
    putStrLn ""
    case namesAndMagnets page of
         Left err -> putStrLn err
         Right rs -> mapM_ putStrLn (map show rs)
    
    ret <- startMagnetDonwload "D:\\Internet\\New folder (4)" $ Magnet "magnet:?xt=urn:btih:38135cb873317dea857c8737a3c860d3139a9448&dn=The.Mentalist.S06E12.720p.HDTV.X264-DIMENSION%5Brartv%5D&tr=udp%3A%2F%2Ftracker.openbittorrent.com%3A80&tr=udp%3A%2F%2Ftracker.publicbt.com%3A80&tr=udp%3A%2F%2Ftracker.istole.it%3A6969&tr=udp%3A%2F%2Ftracker.ccc.de%3A80&tr=udp%3A%2F%2Fopen.demonii.com%3A1337"
    
    putStrLn $ show ret
