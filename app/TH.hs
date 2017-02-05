{-# LANGUAGE TemplateHaskell #-}

module TH
( makeSuffixLenses
) where

import ClassyPrelude

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lib as TH

import Lens.Micro ((&), (.~))
import Lens.Micro.TH (DefName(..), makeLensesWith, lensRules, lensField)

-- | A 'FieldNamer' that strips the _ off of the field name,
-- lowercases the name, add 'L' as a suffix and skips the field if it doesn't start with
-- an '_'.
-- underscoreNoPrefixSuffixNamer :: TH.FieldNamer
underscoreNoPrefixSuffixNamer _ _ n =
  case TH.nameBase n of
    '_':x:xs -> [TopName (TH.mkName ((x:xs) ++ "L"))]
    _        -> []

-- | A template haskell function to build lenses for a record type.
makeSuffixLenses :: TH.Name -> TH.DecsQ
makeSuffixLenses = makeLensesWith $
  lensRules & lensField .~ underscoreNoPrefixSuffixNamer

