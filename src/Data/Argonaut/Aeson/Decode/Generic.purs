module Data.Argonaut.Aeson.Decode.Generic
  ( class DecodeAeson
  , class DecodeAeson'
  , decodeAeson
  , decodeAeson'
  , genericDecodeAeson
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Aeson.Options (Options(Options), SumEncoding(..))
import Data.Argonaut.Core (Json, caseJson, fromBoolean, fromNumber, fromObject, fromString, jsonNull, toObject, toString)
import Data.Argonaut.Decode.Generic.Rep (class DecodeRepArgs, decodeRepArgs)
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep as Rep
import Data.Maybe (Maybe(..))
import Foreign.Object as SM
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

class DecodeAeson r where
  decodeAeson :: Options -> Json -> Either String r

instance decodeAesonNoConstructors :: DecodeAeson Rep.NoConstructors where
  decodeAeson _ _ = Left "Cannot decode empty data type"

instance decodeAesonSum :: DecodeAeson' (Rep.Sum a b) => DecodeAeson (Rep.Sum a b) where
  decodeAeson = decodeAeson'

instance decodeAesonConstructor :: (IsSymbol name, DecodeRepArgs a) => DecodeAeson (Rep.Constructor name a) where
  decodeAeson (Options options) j =
    if options.tagSingleConstructors
    then decodeAeson' (Options options) j
    else do
      let name = reflectSymbol (SProxy :: SProxy name)
      let decodingErr msg = "When decoding a " <> name <> ": " <> msg
      {init, rest} <- let values = toJsonArray j in lmap decodingErr $ decodeRepArgs values
      pure $ Rep.Constructor init

class DecodeAeson' r where
  decodeAeson' :: Options -> Json -> Either String r

instance decodeAesonNoConstructors' :: DecodeAeson' Rep.NoConstructors where
  decodeAeson' _ _ = Left "Cannot decode empty data type"

instance decodeAesonSum' :: (DecodeAeson' a, DecodeAeson' b) => DecodeAeson' (Rep.Sum a b) where
  decodeAeson' o j = Rep.Inl <$> decodeAeson' o j <|> Rep.Inr <$> decodeAeson' o j

toJsonArray :: Json -> Array Json
toJsonArray = caseJson
  (const $ singleton jsonNull)
  (singleton <<< fromBoolean)
  (singleton <<< fromNumber)
  (singleton <<< fromString)
  (\x -> x)
  (singleton <<< fromObject)

instance decodeAesonConstructor' :: (IsSymbol name, DecodeRepArgs a) => DecodeAeson' (Rep.Constructor name a) where
  decodeAeson' (Options { sumEncoding: TaggedObject r }) j = do
    let name = reflectSymbol (SProxy :: SProxy name)
    let decodingErr msg = "When decoding a " <> name <> ": " <> msg
    jObj <- note (decodingErr "expected an object") (toObject j)
    jTag <- note (decodingErr (show r.tagFieldName <> " property is missing")) (SM.lookup r.tagFieldName jObj)
    tag <- note (decodingErr (show r.tagFieldName <> " property is not a string")) (toString jTag)
    when (tag /= name) $
      Left $ decodingErr "'tag' property has an incorrect value"
    {init, rest} <- case SM.lookup r.contentsFieldName jObj of
      Just jValue ->
        let values = toJsonArray jValue
        in lmap decodingErr $ decodeRepArgs values
      Nothing -> do
        let jObj' = SM.delete r.tagFieldName jObj
        lmap decodingErr $ decodeRepArgs $ singleton (fromObject jObj')
    pure $ Rep.Constructor init

-- | Decode `Json` Aeson representation of a value which has a `Generic` type.
genericDecodeAeson :: forall a r. Rep.Generic a r => DecodeAeson r => Options -> Json -> Either String a
genericDecodeAeson o = map Rep.to <<< decodeAeson o
