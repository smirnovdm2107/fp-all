{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains name '[] = 'False
  Contains name (name : rest) = 'True
  Contains name (other : rest) = Contains name rest


type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete name '[] = '[]
  Delete name (name : rest) = Delete name rest
  Delete name (other : rest) = (other : Delete name rest) 

type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add name '[] = '[name]
  Add name (name : rest) = (name : rest)
  Add name (other : rest) = (other : Add name rest) 
