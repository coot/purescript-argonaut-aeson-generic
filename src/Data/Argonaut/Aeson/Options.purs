module Data.Argonaut.Aeson.Options
  ( SumEncoding(..)
  , Options(Options)
  , defaultOptions
  ) where

import Data.Generic.Rep (Constructor, NoArguments, Sum)
data SumEncoding
  = TaggedObject { tagFieldName :: String, contentsFieldName :: String }

newtype Options = Options { sumEncoding :: SumEncoding }

defaultOptions :: Options
defaultOptions = Options
  { sumEncoding: TaggedObject { tagFieldName: "tag", contentsFieldName: "contents" } }
