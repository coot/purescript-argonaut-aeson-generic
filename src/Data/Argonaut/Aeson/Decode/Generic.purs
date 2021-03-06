module Data.Argonaut.Aeson.Decode.Generic
  ( class DecodeAeson
  , class DecodeAeson'
  , decodeAeson
  , decodeAeson'
  , genericDecodeAeson
  ) where

import Prelude (Unit, bind, const, discard, identity, map, pure, show, unit, ($), (/=), (<$>), (<<<), (<>), (==), (>=>))

import Control.Alt ((<|>))
import Data.Argonaut.Aeson.Helpers (class AreAllConstructorsNullary, class IsSingleConstructor, Mode(..), areAllConstructorsNullary, isSingleConstructor)
import Data.Argonaut.Aeson.Options (Options(Options), SumEncoding(..))
import Data.Argonaut.Core (Json, caseJson, caseJsonArray, caseJsonString, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull, toObject, toString)
import Data.Argonaut.Decode.Generic (class DecodeRepArgs, decodeRepArgs)
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Argonaut.Decode.Error (JsonDecodeError(Named, MissingValue, TypeMismatch))
import Data.Either (Either(..), note)
import Data.Generic.Rep as Rep
import Data.Maybe (Maybe(..))
import Foreign.Object as Foreign
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))
import Partial.Unsafe

class DecodeAeson r where
  decodeAeson :: Options -> Json -> Either JsonDecodeError r

instance decodeAesonNoConstructors :: DecodeAeson Rep.NoConstructors where
  decodeAeson _ _ = Left $ Named "Cannot decode empty data type" MissingValue

instance decodeAesonConstructor
  :: ( DecodeRepArgs a
     , IsSymbol name
     , AreAllConstructorsNullary (Rep.Constructor name a)
     , IsSingleConstructor (Rep.Constructor name a)
     , DecodeAeson' (Rep.Constructor name a)
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
  decodeAeson' :: Mode -> Options -> Json -> Either JsonDecodeError r

instance decodeAesonNoConstructors' :: DecodeAeson' Rep.NoConstructors where
  decodeAeson' _ _ _ = Left $ Named "Cannot decode empty data type" MissingValue

instance decodeAesonSum' :: (DecodeAeson' a, DecodeAeson' b) => DecodeAeson' (Rep.Sum a b) where
  decodeAeson' mode o j = Rep.Inl <$> decodeAeson' mode o j <|> Rep.Inr <$> decodeAeson' mode o j

toJsonArray :: Json -> Array Json
toJsonArray = caseJson
  (const $ singleton jsonNull)
  (singleton <<< fromBoolean)
  (singleton <<< fromNumber)
  (singleton <<< fromString)
  (singleton <<< fromArray)
  (singleton <<< fromObject)

toJsonArrayProduct :: Json -> Array Json
toJsonArrayProduct = caseJson
  (const $ singleton jsonNull)
  (singleton <<< fromBoolean)
  (singleton <<< fromNumber)
  (singleton <<< fromString)
  identity
  (singleton <<< fromObject)

decodingErr :: String -> JsonDecodeError -> JsonDecodeError
decodingErr name msg = Named ("When decoding a " <> name) msg

checkTag :: String -> String -> Foreign.Object Json -> Either JsonDecodeError Unit
checkTag tagFieldName expectedTag
  = note (Named (show tagFieldName <> " property is missing") MissingValue) <<< Foreign.lookup tagFieldName
 >=> note (TypeMismatch $ show tagFieldName <> " property is not a string") <<< toString
 >=> \ actualTag -> if actualTag /= expectedTag
   then Left (Named "'tag' property has an incorrect value" (TypeMismatch actualTag))
   else Right unit

instance decodeAesonConstructorNoArguments' :: IsSymbol name => DecodeAeson' (Rep.Constructor name (Rep.NoArguments)) where
  decodeAeson' mode options json =
    let name = reflectSymbol (SProxy :: SProxy name)
    in lmap (decodingErr name) case {mode: mode, options: options} of

        { mode: Mode {_Mode_ConstructorIsSingle: true}
        , options: Options {tagSingleConstructors: false}
        } -> case caseJsonArray Nothing Just json of
          Just [ ] -> Right (Rep.Constructor Rep.NoArguments)
          _ -> Left $ TypeMismatch "Expected an empty array!"

        { mode: Mode {_Mode_ConstructorsAreAllNullary: true}
        , options: Options {allNullaryToStringTag: true}
        } -> case caseJsonString Nothing Just json of
          Nothing -> Left $ TypeMismatch "Expected a string!"
          Just tag -> if tag == name
            then Right (Rep.Constructor Rep.NoArguments)
            else Left $ TypeMismatch "Mismatched constructor tag!"

        _ -> decodeGeneralCase mode options json

instance decodeAesonConstructorProduct' :: (IsSymbol name, DecodeRepArgs a, DecodeRepArgs b) => DecodeAeson' (Rep.Constructor name (Rep.Product a b)) where
  decodeAeson' mode options json =
    let name = reflectSymbol (SProxy :: SProxy name)
    in lmap (decodingErr name) case {mode: mode, options: options} of

        { mode: Mode {_Mode_ConstructorIsSingle: true}
        , options: Options {tagSingleConstructors: false}
        } -> do
            {init, rest} <- (decodeRepArgs <<< caseJsonArray (singleton json) identity) json
            pure (Rep.Constructor init)

        { options: Options {sumEncoding: TaggedObject taggedObject}
        } -> do
          objectJson <- (note (TypeMismatch "expected an object") <<< toObject) json
          checkTag taggedObject.tagFieldName name objectJson
          {init, rest} <- case Foreign.lookup taggedObject.contentsFieldName objectJson of
            Just contents -> -- This must be an ordinary constructor.
              (decodeRepArgs <<< toJsonArrayProduct) contents
            Nothing -> -- This must be a record constructor.
              (decodeRepArgs <<< singleton <<< fromObject <<< Foreign.delete taggedObject.tagFieldName) objectJson
          pure (Rep.Constructor init)

instance decodeAesonConstructor' :: (IsSymbol name, DecodeRepArgs (Rep.Argument a)) => DecodeAeson' (Rep.Constructor name (Rep.Argument a)) where
  decodeAeson' mode options json =
    let name = reflectSymbol (SProxy :: SProxy name)
    in lmap (decodingErr name) case {mode: mode, options: options} of

        { mode: Mode {_Mode_ConstructorsAreAllNullary: true}
        } -> unsafeCrashWith "Unreachable: cannot have all nullary constructors and an `Argument` constructor at once."

        { mode: Mode {_Mode_ConstructorIsSingle: true}
        , options: Options {tagSingleConstructors: false}
        } -> do
            {init, rest} <- (decodeRepArgs <<< caseJsonArray (singleton json) (singleton <<< fromArray)) json
            pure (Rep.Constructor init)

        _ -> decodeGeneralCase mode options json

decodeGeneralCase :: forall name a. IsSymbol name => DecodeRepArgs a => Mode -> Options -> Json -> Either JsonDecodeError (Rep.Constructor name a)
decodeGeneralCase mode options json =
  let name = reflectSymbol (SProxy :: SProxy name)
  in case {mode: mode, options: options} of
        { options: Options {sumEncoding: TaggedObject taggedObject}
        } -> do
          objectJson <- (note (TypeMismatch "expected an object") <<< toObject) json
          checkTag taggedObject.tagFieldName name objectJson
          {init, rest} <- case Foreign.lookup taggedObject.contentsFieldName objectJson of
            Just contents -> -- This must be an ordinary constructor.
              (decodeRepArgs <<< toJsonArray) contents
            Nothing -> -- This must be a record constructor.
              (decodeRepArgs <<< singleton <<< fromObject <<< Foreign.delete taggedObject.tagFieldName) objectJson
          pure (Rep.Constructor init)

-- | Decode `Json` Aeson representation of a value which has a `Generic` type.
genericDecodeAeson :: forall a r. Rep.Generic a r => DecodeAeson r => Options -> Json -> Either JsonDecodeError a
genericDecodeAeson o = map Rep.to <<< decodeAeson o
