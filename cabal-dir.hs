module Main(main) where

import Distribution.Simple.LocalBuildInfo(
	LocalBuildInfo, CopyDest(..), absoluteInstallDirs)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Control.Applicative

import Distribution.PackageDescription.Parse
import Distribution.PackageDescription
import Distribution.Verbosity

import System.Directory
import Data.List

data InstallDirs = InstallDirs {
	prefix :: FilePath,
	bindir :: FilePath,
	libdir :: FilePath,
	datadir :: FilePath,
	htmldir :: FilePath
 } deriving Show

getLocalBuildInfo :: IO LocalBuildInfo
getLocalBuildInfo = read . (!! 1) . lines <$> readFile "./dist/setup-config"

getPackageDescription :: IO PackageDescription
getPackageDescription = packageDescription <$>
	(readPackageDescription silent =<< getCabalFile)

getCabalFile :: IO FilePath
getCabalFile =
	head . filter (".cabal" `isSuffixOf`) <$> getDirectoryContents "."

getInstallDirs :: IO InstallDirs
getInstallDirs = do
	lbi <- getLocalBuildInfo
	pd <- getPackageDescription
	let dirs = absoluteInstallDirs pd lbi NoCopyDest
	return $ InstallDirs {
		prefix = LBI.prefix dirs,
		bindir = LBI.bindir dirs,
		libdir = LBI.libdir dirs,
		datadir = LBI.datadir dirs,
		htmldir = LBI.htmldir dirs
	 }

showInstallDirsGen :: (FilePath -> String) -> InstallDirs -> String
showInstallDirsGen sw InstallDirs {
	prefix = px,
	bindir = bd,
	libdir = ld,
	datadir = dd,
	htmldir = hd } =
	"prefix : " ++ px ++ "/\n" ++
	"bindir : " ++ sw bd ++ "\n" ++
	"libdir : " ++ sw ld ++ "\n" ++
	"datadir: " ++ sw dd ++ "\n" ++
	"htmldir: " ++ sw hd ++ "\n"

showRPInstallDirs :: FilePath -> InstallDirs -> String
showRPInstallDirs = showInstallDirsGen . removePrefix

printRPInstallDirs :: FilePath -> InstallDirs -> IO ()
printRPInstallDirs p = putStr . showRPInstallDirs p

removePrefix :: FilePath -> FilePath -> String
removePrefix "" s = s
removePrefix p s
	| p `isPrefixOf` s = dropWhile (== '/') $ drop (length p) s
	| otherwise = s

main :: IO ()
main = do
	dirs <- getInstallDirs
	printRPInstallDirs (prefix dirs) dirs
