module Data.Argonaut.Aeson.Options
  ( SumEncoding(..)
  , Options(Options)
  , defaultOptions

  , class IsAllNullary
  ) where

import Data.Generic.Rep (Constructor, NoArguments, Sum)
data SumEncoding
  = TaggedObject { tagFieldName :: String, contentsFieldName :: String }

newtype Options = Options
  { allNullaryToStringTag :: Boolean
  , sumEncoding :: SumEncoding
  }

defaultOptions :: Options
defaultOptions = Options
  { allNullaryToStringTag: true
  , sumEncoding: TaggedObject { tagFieldName: "tag", contentsFieldName: "contents" }
  }

-- | Do not provide instances for this class.
class IsAllNullary a

instance noArgumentsIsAllNullary :: IsAllNullary NoArguments
instance constructorNoArgumentsIsAllNullary :: IsAllNullary (Constructor n NoArguments)
instance sumAllNullary :: (IsAllNullary a, IsAllNullary b) => IsAllNullary (Sum a b)
