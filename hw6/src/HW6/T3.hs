{-# LANGUAGE LambdaCase #-}
module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid

  , simulate
  ) where

import           System.Random   (Random (randomR), RandomGen (split), StdGen,
                                  mkStdGen)

import           Control.Comonad
import           Control.Monad   (liftM2)
import           Data.Grid       (Grid (..), down, gridWrite, left, right, up)
import           Data.ListZipper (genericMove)

data Config = Config
  { probability      :: Double
  , incubationPeriod :: Int
  , illnessDuration  :: Int
  , immunityDuration :: Int
  } deriving Show

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving Show

data Cell = Cell
  { cellState :: CellState
  , cellRand  :: StdGen
  }

type Comonad19Grid = Grid Cell

createCell :: Seed -> Cell
createCell seed = Cell {cellState = Healthy, cellRand = mkStdGen seed}

neighbors :: [Grid a -> Grid a]
neighbors = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
 where
  horizontals = [left, right]
  verticals = [up, down]

infectedCount :: [Cell] -> Int
infectedCount = length . filter isZombie
  where
    isZombie :: Cell -> Bool
    isZombie (Cell state _) = case state of
      Infected _ -> True
      Ill _      -> True
      _          -> False

infectedNeighbors :: Grid Cell -> Int
infectedNeighbors grid = infectedCount $ map (\direction -> extract $ direction grid) neighbors

stateStep :: Config -> CellState -> CellState
stateStep config = \case
  Healthy       -> Healthy
  Infected 1    -> Ill illDays
  Infected days -> Infected (days - 1)
  Ill 1         -> Immune immunDays
  Ill days      -> Ill (days - 1)
  Immune 1      -> Healthy
  Immune days   -> Immune (days - 1)
  where
    immunDays = immunityDuration config
    illDays = illnessDuration config

tryInfectCell :: Int -> Config -> Cell -> Cell
tryInfectCell 0 _ cell = cell
tryInfectCell n config cell@(Cell state gen) = case state of
  Healthy -> if prob' <= prob then cell {cellState = Infected infectedDays, cellRand = gen'} else tryInfectCell (n - 1) config cell { cellRand = gen' }
  _ -> cell
  where
    (prob', gen') = randomR (0, 1) gen
    infectedDays = incubationPeriod config
    prob = probability config

rule :: Config -> Grid Cell -> Cell
rule config grid = tryInfectCell (infectedNeighbors grid) config curCell
  where
    (Cell state gen) = extract grid
    curCell = Cell (stateStep config state) gen

createEmptyState :: Seed -> Comonad19Grid
createEmptyState seed = Grid outerLZ
  where
    outerLZ = genericMove leftListZipperCreator rightListZipperCreator innerLZ
    innerLZ = genericMove leftCellsCreator rightCellsCreator (createCell seed)
    leftListZipperCreator = (<$>) rightCellsCreator
    rightListZipperCreator = (<$>) leftCellsCreator

leftCellsCreator :: Cell -> Cell
leftCellsCreator = cellsCreator fst

rightCellsCreator :: Cell -> Cell
rightCellsCreator = cellsCreator snd

cellsCreator :: ((StdGen, StdGen) -> StdGen) -> Cell -> Cell
cellsCreator part cell = cell { cellRand = part $ split newGen}
  where (newGen, _) = split (cellRand cell)

firstState :: Seed -> Config -> Comonad19Grid
firstState seed config = gridWrite (createCell seed) {cellState = Infected (incubationPeriod config)} emptyGrid
  where
    emptyGrid = createEmptyState seed

evolve :: Config -> Comonad19Grid -> Comonad19Grid
evolve config grid = grid =>> rule config

type Seed = Int

simulate :: Seed -> Config -> [Comonad19Grid]
simulate seed config = iterate (evolve config) (firstState seed config)


instance Show Cell where
  show (Cell state _) = case state of
    Healthy      -> "_"
    (Infected _) -> "i"
    (Ill _)      -> "#"
    (Immune _)   -> "@"
