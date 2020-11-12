module Data.Argonaut.Aeson.Options
  ( SumEncoding(..)
  , Options(Options)
  , defaultOptions
  ) where

import Prelude (class Show)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data SumEncoding
  = TaggedObject { tagFieldName :: String, contentsFieldName :: String }

derive instance generic_SumEncoding :: Generic SumEncoding _

instance show_SumEncoding ::Show SumEncoding where
  show = genericShow

newtype Options = Options
  { sumEncoding :: SumEncoding
  , tagSingleConstructors ∷ Boolean
  , allNullaryToStringTag :: Boolean
  }

derive instance generic_Options :: Generic Options _

instance show_Options ::Show Options where
  show = genericShow

defaultOptions :: Options
defaultOptions = Options
  { sumEncoding: TaggedObject { tagFieldName: "tag", contentsFieldName: "contents" }
  , tagSingleConstructors: false
  , allNullaryToStringTag: true
  }
