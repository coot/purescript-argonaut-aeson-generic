module Data.Argonaut.Aeson.Decode.Generic
  ( class DecodeAeson
  , class DecodeAeson'
  , decodeAeson
  , decodeAeson'
  , genericDecodeAeson
  ) where

import Prelude (bind, const, discard, map, pure, show, when, ($), (/=), (<$>), (<<<), (<>), (==))

import Control.Alt ((<|>))
import Data.Argonaut.Aeson.Helpers (class AreAllConstructorsNullary, class IsSingleConstructor, Mode(..), areAllConstructorsNullary, isSingleConstructor)
import Data.Argonaut.Aeson.Options (Options(Options), SumEncoding(..))
import Data.Argonaut.Core (Json, caseJson, caseJsonArray, caseJsonString, fromBoolean, fromNumber, fromObject, fromString, jsonNull, toObject, toString)
import Data.Argonaut.Decode.Generic.Rep (class DecodeRepArgs, decodeRepArgs)
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep as Rep
import Data.Maybe (Maybe(..))
import Foreign.Object as SM
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))

class DecodeAeson r where
  decodeAeson :: Options -> Json -> Either String r

instance decodeAesonNoConstructors :: DecodeAeson Rep.NoConstructors where
  decodeAeson _ _ = Left "Cannot decode empty data type"

instance decodeAesonConstructor
  :: ( DecodeRepArgs a
     , IsSymbol name
     , AreAllConstructorsNullary (Rep.Constructor name a)
     , IsSingleConstructor (Rep.Constructor name a)
     )
  => DecodeAeson (Rep.Constructor name a) where
  decodeAeson o thing = decodeAeson' mode o thing
    where
      mode = Mode
        { _Mode_ConstructorIsSingle: isSingleConstructor (Proxy :: Proxy (Rep.Constructor name a))
        , _Mode_ConstructorsAreAllNullary: areAllConstructorsNullary (Proxy :: Proxy (Rep.Constructor name a))
        }

instance decodeAesonSum
  :: ( DecodeAeson' (Rep.Sum a b)
     , AreAllConstructorsNullary (Rep.Sum a b)
     , IsSingleConstructor (Rep.Sum a b)
     )
  => DecodeAeson (Rep.Sum a b) where
  decodeAeson o thing = decodeAeson' mode o thing
    where
      mode = Mode
        { _Mode_ConstructorIsSingle: isSingleConstructor (Proxy :: Proxy (Rep.Sum a b))
        , _Mode_ConstructorsAreAllNullary: areAllConstructorsNullary (Proxy :: Proxy (Rep.Sum a b))
        }

class DecodeAeson' r where
  decodeAeson' :: Mode -> Options -> Json -> Either String r

instance decodeAesonNoConstructors' :: DecodeAeson' Rep.NoConstructors where
  decodeAeson' _ _ _ = Left "Cannot decode empty data type"

instance decodeAesonSum' :: (DecodeAeson' a, DecodeAeson' b) => DecodeAeson' (Rep.Sum a b) where
  decodeAeson' mode o j = Rep.Inl <$> decodeAeson' mode o j <|> Rep.Inr <$> decodeAeson' mode o j

toJsonArray :: Json -> Array Json
toJsonArray = caseJson
  (const $ singleton jsonNull)
  (singleton <<< fromBoolean)
  (singleton <<< fromNumber)
  (singleton <<< fromString)
  (\x -> x)
  (singleton <<< fromObject)

instance decodeAesonConstructor' :: (IsSymbol name, DecodeRepArgs a) => DecodeAeson' (Rep.Constructor name a) where
  decodeAeson' mode options json =
    let name = reflectSymbol (SProxy :: SProxy name)
        decodingErr msg = "When decoding a " <> name <> ": " <> msg
    in case {mode: mode, options: options} of

        { mode: Mode {_Mode_ConstructorIsSingle: true, _Mode_ConstructorsAreAllNullary: true}
        , options: Options {tagSingleConstructors: false, allNullaryToStringTag: true}
        } -> case caseJsonArray Nothing (Just <<< decodeRepArgs) json of
          Nothing -> Left (decodingErr ("Expected an empty array!"))
          Just (Left errorMessage) -> Left (decodingErr errorMessage)
          Just (Right {init: x, rest: _}) -> Right (Rep.Constructor x)
            -- We know that `x ≡ Rep.NoArguments`,
            -- but we cannot supply the value directly because the method is polymorphic.

        { mode: Mode {_Mode_ConstructorsAreAllNullary: true}
        , options: Options {allNullaryToStringTag: true}
        } -> case caseJsonString Nothing Just json of
          Nothing -> Left (decodingErr ("Expected a string!"))
          Just tag -> if tag == name
            then map (Rep.Constructor <<< (_.init)) (decodeRepArgs [ ])
              -- We know that it should be `Rep.Constructor Rep.NoArguments`,
              -- but we cannot supply the value directly because the method is polymorphic.
            else Left "Mismatched constructor tag!"

        { mode: Mode {_Mode_ConstructorIsSingle: true}
        , options: Options {tagSingleConstructors: false}
        } -> do
            {init, rest} <- let values = toJsonArray json in lmap decodingErr $ decodeRepArgs values
            pure $ Rep.Constructor init

        { options: Options {sumEncoding: TaggedObject taggedObject}
        } -> do
          jObj <- note (decodingErr "expected an object") (toObject json)
          jTag <- note (decodingErr (show taggedObject.tagFieldName <> " property is missing")) (SM.lookup taggedObject.tagFieldName jObj)
          tag <- note (decodingErr (show taggedObject.tagFieldName <> " property is not a string")) (toString jTag)
          when (tag /= name) $
            Left $ decodingErr "'tag' property has an incorrect value"
          {init, rest} <- case SM.lookup taggedObject.contentsFieldName jObj of
            Just jValue ->
              let values = toJsonArray jValue
              in lmap decodingErr $ decodeRepArgs values
            Nothing -> do
              let jObj' = SM.delete taggedObject.tagFieldName jObj
              lmap decodingErr $ decodeRepArgs $ singleton (fromObject jObj')
          pure $ Rep.Constructor init

-- | Decode `Json` Aeson representation of a value which has a `Generic` type.
genericDecodeAeson :: forall a r. Rep.Generic a r => DecodeAeson r => Options -> Json -> Either String a
genericDecodeAeson o = map Rep.to <<< decodeAeson o
