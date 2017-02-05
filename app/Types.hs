module Types
( AppException(..)
) where


import ClassyPrelude

import Control.Exception.Safe (Exception(..))

data AppException = MPDException | UnknownException deriving (Show, Typeable)
instance Exception AppException
