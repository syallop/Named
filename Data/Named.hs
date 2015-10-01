{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , KindSignatures
  , MultiParamTypeClasses
  , TypeOperators
  , UndecidableInstances
  , OverlappingInstances
  #-}
{-|
Module      : Data.Named
Copyright   : (c) Samuel A. Yallop, 2015
Maintainer  : syallop@gmail.com
Stability   : experimental

Associate type-level names (using GHC.TypeLits (Symbol)) to values in a heterogeneous non-empty list.

Example:

@
  let -- Some values
      f b = if b then 'a' else 'z'
      b   = True

      -- Associate type-level Names with values
      namedF = Named (Name :: Name "fName", f)
      namedB = Named (Name :: Name "bName", b)

      -- Create a hetrogeneous non-empty list of named values
      -- nameds :: Nameds '[Named "fName" (Bool -> Char), Named "bName" Bool]
      nameds = namedF `And` (One namedB)

     -- Lookup and work with values by their type-level Name.
     in let f = lookupNamed nameds (Name :: Name "fName")
            b = lookupNamed nameds (Name :: Name "bName")
           in f b
@

> 'a'

-}
module Data.Named
  ( Name(Name)
  , Named(Named)
  , Nameds(One,And)
  , LookupNamed(lookupNamed)
  ) where

import GHC.TypeLits

-- | Proxy a name type with a value
-- E.G.
--
-- let n = Name :: Name "name"
data Name (n :: Symbol) = Name

-- | A value of type 't', with name 'n :: Symbol'
newtype Named n t = Named {unNamed :: (Name n,t)}

-- | A collection of 'Named's collecting name and type parameters in the type list.
data Nameds (xs :: [*]) where

  -- | A single 'Named'
  One
    :: Named n t
    -> Nameds '[Named n t]

  -- | Many 'Named'
  And
    :: Named n t
    -> Nameds xs
    -> Nameds ((Named n t) ': xs)

-- | Lookup the value with name 'n' typed 't' within a 'Named xs'.
--
-- Note: If multiple values are associated with the same name, the first is picked.
--

-- | Within a name,type association 'xs', 'n' exists determined to have type 't'.
class LookupNamed
  (xs :: [*])
  (n  :: Symbol)
  (t  :: *)
  | xs n -> t
  where
  -- | Lookup the value with Name 'n' (determined to have type 't') within a 'Nameds xs'
  -- Note: If multiple values are associated with the same name, the first is picked.
  lookupNamed :: Nameds xs -> Name n -> t

-- Lookup in singleton list
instance LookupNamed '[(Named n t)] n t where
  lookupNamed nameds _ = case nameds of
    One (Named (_,t)) -> t

-- At head of list
instance LookupNamed ((Named n t) ': xs) n t where
  lookupNamed nameds _ = case nameds of
    And (Named (_, t)) _ -> t

-- Recurse into list
instance LookupNamed xs n t =>
         LookupNamed ((Named y z) ': xs) n t where
  lookupNamed nameds name = case nameds of
    And _ xs -> lookupNamed xs name

test = let f = lookupNamed nameds (Name :: Name "named1")
           x = lookupNamed nameds (Name :: Name "named2")
          in f x
  where
    nameds = named1 `And` (One named2)

    named1 = let f b = if b then 'a' else 'z'
              in Named (Name :: Name "named1", f)

    named2 = let x = True
              in Named (Name :: Name "named2", x)

