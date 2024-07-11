module Main (main) where

import HW6.T3 (Comonad19Grid, Config(..), Cell(..), simulate)
import qualified Data.Grid as DG (toList)
import Control.Monad ( when )
import Control.Exception (throwIO, Exception)
import Options.Applicative

stdGenSeed :: Int
stdGenSeed = 427160503397

main :: IO ()
main = do
    opts <- execParser $ info optsParser mempty
    let
        config = Config (optProb opts) (optIncub opts) (optIll opts) (optImmun opts)
        fieldSize = optGridSize opts
        iters = optIterations opts
    validate iters fieldSize config
    comonad19 iters fieldSize config

validate :: Iterations -> FieldSize -> Config -> IO ()
validate iters fieldSize config = do
    foldMap validatePositive [fieldSize, iters, immunityDuration config, illnessDuration config, incubationPeriod config]
    validateProb (probability config)
  where
    validatePositive :: (Num a, Ord a) => a -> IO ()
    validatePositive a = when (a <= 0) $ throwIO IllegalArgumentException

    validateProb :: Double -> IO ()
    validateProb p = when ((p < 0) || (1 < p)) $ throwIO IllegalArgumentException

optsParser :: Parser Opts
optsParser =  
    Opts 
        <$> parseProb
        <*> parseIncub
        <*> parseIll
        <*> parseImmun
        <*> parseGridSize
        <*> parseIterations
    where
        parseProb :: Parser Double
        parseProb = base $ long "prob" <> metavar "\'float number from 0.0 to 1.1\'"

        parseIncub :: Parser Int
        parseIncub = intFlag "incub"

        parseIll :: Parser Int 
        parseIll = intFlag "ill"

        parseImmun :: Parser Int
        parseImmun = intFlag "immun"

        parseGridSize :: Parser Int
        parseGridSize = intFlag "grid-size"

        parseIterations :: Parser Int
        parseIterations = intFlag "iterations"

        intFlag :: (Read a) => String -> Parser a
        intFlag flagName = base $ long flagName <> metavar "int"

        base :: (Read a) => Mod OptionFields a -> Parser a
        base = option auto



comonad19 :: Iterations -> FieldSize -> Config -> IO ()
comonad19 iters fieldSize config = printStates iters fieldSize (simulate stdGenSeed config)

printStates :: Iterations -> FieldSize -> [Comonad19Grid] -> IO ()
printStates iters fieldSize (state : rest)  = when (iters > 0) $ do
    putStrLn $ printField $ DG.toList state fieldSize
    printStates (iters - 1) fieldSize rest

printStates _ _ [] = throwIO IllegalStateException

printField :: [[Cell]] -> String
printField (cur : rest) = processRow cur ++ "\n" ++ printField rest

    where
        processRow :: [Cell] -> String
        processRow (cell : rest') = show cell ++ processRow rest'
        processRow [] = ""
printField [] = ""

data Opts = Opts
    { optProb :: Double
    , optIncub :: Int
    , optIll :: Int
    , optImmun :: Int
    , optGridSize :: Int
    , optIterations :: Int
    }


type FieldSize = Int
type Iterations = Int

data Comonad19Excetion = IllegalStateException
    | IllegalArgumentException deriving (Show)

instance Exception Comonad19Excetion
