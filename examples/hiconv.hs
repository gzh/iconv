{-
 - This example is similar to the commandline iconv program.
 - Author: Conrad Parker, July 2007

  Usage: hiconv [options] filename

    -h, -?       --help, --usage       Display this help and exit
    -f encoding  --from-code=encoding  Convert characters from encoding
    -t encoding  --to-code=encoding    Convert characters to encoding
    -o file      --output=file         Specify output file (instead of stdout)

 -}

module Main where

import qualified Data.ByteString.Lazy as Lazy

import Control.Monad

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import System.Exit

import Codec.Text.IConv

------------------------------------------------------------
-- main
--

main :: IO ()
main = do
    args <- getArgs
    (config, filenames) <- processArgs args

    let inputFile = head filenames
    input <- case inputFile of
        "-" -> Lazy.getContents
        _   -> Lazy.readFile inputFile

    let output = convert (fromEncoding config) (toEncoding config) input
        o = outputFile config

    case o of
        "-" -> Lazy.putStr output
        _   -> Lazy.writeFile o output

------------------------------------------------------------
-- Option handling
--

data Config =
    Config {
        fromEncoding :: String,
        toEncoding :: String,
        outputFile :: FilePath
    }

defaultConfig =
    Config {
        fromEncoding = "",
        toEncoding = "",
        outputFile = "-"
    }

data Option = Help
            | FromEncoding String
            | ToEncoding String
            | OutputFile String
            deriving Eq

options :: [OptDescr Option]
options = [ Option ['h', '?'] ["help", "usage"] (NoArg Help)
              "Display this help and exit"
          , Option ['f'] ["from-code"] (ReqArg FromEncoding "encoding")
              "Convert characters from encoding"
          , Option ['t'] ["to-code"] (ReqArg ToEncoding "encoding")
              "Convert characters to encoding"
          , Option ['o'] ["output"] (ReqArg OutputFile "file")
              "Specify output file (instead of stdout)"
          ]

processArgs :: [String] -> IO (Config, [String])
processArgs args = do
    case getOpt Permute options args of
        (opts, args, []) -> do
            processHelp opts
            config <- processConfig defaultConfig opts
            checkConfig (config, args)
            return (config, args)

checkConfig :: (Config, [String]) -> IO ()
checkConfig (config, filenames) = do
    when (any null [fromEncoding config, toEncoding config] || null filenames) $
      processHelp [Help]
    return ()

processHelp :: [Option] -> IO ()
processHelp opts = do
    name <- getProgName
    let header = "\nUsage: " ++ name ++ " [options] filename\n"
    when (Help `elem` opts) $ do
        putStrLn $ usageInfo header options
        exitWith ExitSuccess
    return ()

processConfig :: Config -> [Option] -> IO Config
processConfig = foldM processOneOption
    where
        processOneOption config (FromEncoding f) =
            return $ config {fromEncoding = f}
        processOneOption config (ToEncoding t) =
            return $ config {toEncoding = t}
        processOneOption config (OutputFile o) =
            return $ config {outputFile = o}
