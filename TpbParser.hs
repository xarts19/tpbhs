module TpbParser
( parseResultsPage
) where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Applicative
import Text.HTML.TagSoup
import Torrent (Magnet(..), Bytes(..), Torrent, torrentFromTuple)

type Page = String
type Error = String

extract :: String -> [(String, String)] -> [Tag String] -> [[Tag String]]
extract tag attributes tags = map (takeWhile (/= TagClose tag)) $ sections (~== TagOpen tag attributes) tags

extractTag :: String -> [Tag String] -> [[Tag String]]
extractTag tag tags = extract tag [] tags

findTagText :: [Tag String] -> Maybe String
findTagText tags = fmap fromTagText $ find isTagText tags

parseTable :: String -> Either Error [Tag String]
parseTable page = case listToMaybe $ extract "table" [("id", "searchResult")] $ parseTags page of
                       Nothing -> Left "No table found"
                       Just t -> Right t

parseRows :: [Tag String] -> [[Tag String]]
parseRows table = extractTag "tr" table

parseColumns :: [Tag String] -> [[Tag String]]
parseColumns singleRow = extractTag "td" singleRow

decodeName :: [Tag String] -> Maybe String
decodeName descr_tags = needed_tag >>= findTagText
    where
        needed_tag = listToMaybe $ extract "a" [("class", "detLink")] descr_tags

decodeMagnet :: [Tag String] -> Maybe Magnet
decodeMagnet descr_tags = fmap Magnet $ fmap (fromAttrib "href") $ needed_tag >>= listToMaybe
    where
        needed_tag = listToMaybe $ extract "a" [("title", "Download this torrent using magnet")] descr_tags

findSize :: String -> Maybe String
findSize descr = case afterSize of { "" -> Nothing; str -> Just str }
    where
        afterSize = (takeWhile (/= ',') . unwords . drop 1 . dropWhile (/= "Size") . words) descr

parseSize :: String -> Maybe Bytes
parseSize size_str = fmap Bytes $ fmap round $ parse $ words size_str
    where
        parse (n:mult:[]) = (*) <$> Just (read n :: Double) <*> getMult mult
        parse _ = Nothing
        getMult "GiB" = Just $ (2 :: Double) ^ (30 :: Int)
        getMult "MiB" = Just $ (2 :: Double) ^ (20 :: Int)
        getMult "KiB" = Just $ (2 :: Double) ^ (10 :: Int)
        getMult "B" = Just $ (1 :: Double)
        getMult _ = Nothing

decodeSize :: [Tag String] -> Maybe Bytes
decodeSize descr_tags = needed_tag >>= findTagText >>= findSize >>= parseSize
    where
        needed_tag = listToMaybe $ extract "font" [("class", "detDesc")] descr_tags

decodeInt :: [Tag String] -> Maybe Int
decodeInt int_tags = fmap read $ findTagText int_tags

decodeRow :: [[Tag String]] -> Maybe Torrent
decodeRow (_:descr:s:l:[]) = fmap torrentFromTuple $ (,,,,) <$> decodeName descr <*> decodeMagnet descr <*> decodeSize descr <*> decodeInt s <*> decodeInt l
decodeRow _ = Nothing

parseResultsPage :: Page -> Either Error [Torrent]
parseResultsPage page = liftM catMaybes $ liftM (map decodeRow) columns
    where
        columns = liftM (map parseColumns) $ liftM parseRows $ parseTable page

