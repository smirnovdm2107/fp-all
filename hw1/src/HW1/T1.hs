module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import           Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Eq)

nextDay :: Day -> Day
nextDay d = case d of
  Monday    -> Tuesday
  Tuesday   -> Wednesday
  Wednesday -> Thursday
  Thursday  -> Friday
  Friday    -> Saturday
  Saturday  -> Sunday
  Sunday    -> Monday

afterDays :: Natural -> Day -> Day
afterDays 0 d = d
afterDays c d = afterDays (pred c) (nextDay d)

isWeekend :: Day -> Bool
isWeekend d = case d of
  Saturday -> True
  Sunday   -> True
  _        -> False

daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty d      = daysToParty (nextDay d) + 1
