module UTorrentUtils
( startMagnetDonwload
) where

import System.Process
import System.Exit
import Torrent (Magnet(..))

utorrentCommand :: String
utorrentCommand = "\"C:\\Program Files (x86)\\uTorrent\\uTorrent.exe\" /DIRECTORY"

startMagnetDonwload :: FilePath -> Magnet -> IO (Maybe Int)
startMagnetDonwload destination_folder (Magnet magnet) = do
    (_, _, _, pHandle) <- createProcess $ shell command
    ret <- waitForProcess pHandle
    case ret of
        ExitSuccess -> return Nothing
        ExitFailure code -> return $ Just code
    where
        command = utorrentCommand ++ " \"" ++ destination_folder ++ "\" \"" ++ magnet ++ "\""
