Associate type-level names (using GHC.TypeLits (Symbol)) to values in a heterogeneous non-empty list.

Example:

```haskell
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
```

```haskell
> 'a'
```

