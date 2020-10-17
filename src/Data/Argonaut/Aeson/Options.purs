module Data.Argonaut.Aeson.Options
  ( SumEncoding(..)
  , Options(Options)
  , defaultOptions
  ) where

data SumEncoding
  = TaggedObject { tagFieldName :: String, contentsFieldName :: String }

newtype Options = Options
  { sumEncoding :: SumEncoding
  , tagSingleConstructors ∷ Boolean
  , allNullaryToStringTag :: Boolean
  }

defaultOptions :: Options
defaultOptions = Options
  { sumEncoding: TaggedObject { tagFieldName: "tag", contentsFieldName: "contents" }
  , tagSingleConstructors: false
  , allNullaryToStringTag: true
  }
