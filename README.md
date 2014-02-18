TbpHs
=========

TVShow downloader for thepiratebay.se.

Usage:
------

 1. Create root directory for all shows, we will be calling it ROOT from now on.
 2. Place the executable into ROOT directory
 3. Inside ROOT directory, each subdirectory which has a file named 'last\_<EPISODE>'
    is considered a TV show directory.
 4. Subdirectory's name is used as a serch term on TPB.
 5. The file 'last\_<EPISODE>' is interprited as a last watched show.
 6. This program scans the subdirectory's contents and determines episodes that need
    downloading base on the maximum of episode number in 'last\_<EPISODE>' file's name
	and other files that match the show name and have episode numbers.
 7. uTorrent is started and used to download magnet links from TPB.
	
Example:
--------


	ROOT/
	    big_bang_theory/
    		last_S02E03
    		The.Big.Bang.Theory.S03E15.720p.DIMESION.Blah.Blah.mkv
    		The.Big.Bang.Theory.S03E16.720p.DIMESION.Blah.Blah.mkv
    		The.Big.Bang.Theory.S03E17.720p.DIMESION.Blah.Blah.mkv
    		The.Big.Bang.Theory.S03E18.720p.DIMESION.Blah.Blah.mkv
    		...
	    the_mentalist/
    		last_S05E13
    		...
		
Based on this structure, the latest episode for the Big Bang Theory will be considered S03E18
(max between all present episodes and 'last\_<EPISODE>' file). Similarly, for the mentalist,
the last episode will be S05E13, because it only has 'last\_<EPISODE>' file.
