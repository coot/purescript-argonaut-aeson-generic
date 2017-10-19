module Data.Argonaut.Aeson.Encode.Generic
  ( class EncodeAeson
  , class EncodeRepArgs
  , RepArgsEncoding(..)
  , encodeAeson
  , encodeRepArgs
  , genericEncodeAeson
  ) where

import Prelude

import Data.Argonaut.Aeson.Options (class IsAllNullary, Options(Options), SumEncoding(..))
import Data.Argonaut.Core (Json, JObject, fromArray, fromObject, fromString)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Array (cons, head, length, snoc)
import Data.Generic.Rep as Rep
import Data.Maybe (fromJust)
import Data.StrMap as SM
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Partial.Unsafe (unsafePartial)

class EncodeAeson r where
  encodeAeson :: Options -> r -> Json

instance encodeAesonNoConstructors :: EncodeAeson Rep.NoConstructors where
  encodeAeson o r = encodeAeson o r

instance encodeAesonSum :: (EncodeAeson a, EncodeAeson b) => EncodeAeson (Rep.Sum a b) where
  encodeAeson o (Rep.Inl a) = encodeAeson o a
  encodeAeson o (Rep.Inr b) = encodeAeson o b

data RepArgsEncoding
  = Arg (Array Json)
  | Rec JObject

instance semigroupRepArgsEncoding :: Semigroup RepArgsEncoding where
  append (Arg a) (Arg b) = Arg (a <> b)
  append (Arg a) (Rec b) = Arg (snoc a $ fromObject b)
  append (Rec a) (Arg b) = Arg (cons (fromObject a) b)
  append (Rec a) (Rec b) = Arg [fromObject a, fromObject b]

encodeAllNullaryAsString
  :: forall a name
   . IsAllNullary a
  => IsSymbol name
  => Options
  -> Rep.Constructor name a
  -> Json
encodeAllNullaryAsString (Options { allNullaryToStringTag: true }) _
  = fromString $ reflectSymbol (SProxy :: SProxy name)
encodeAllNullaryAsString (Options { sumEncoding: TaggedObject r }) _
  = fromObject $ SM.insert r.tagFieldName (fromString $ reflectSymbol (SProxy :: SProxy name)) SM.empty

instance encodeAesonAllNullaryConstructor
  :: ( IsSymbol name
     , IsAllNullary a
     , EncodeRepArgs a
     ) => EncodeAeson (Rep.Constructor name a) where
  encodeAeson o a = encodeAllNullaryAsString o a
else instance encodeAesonConstructor
  :: ( IsSymbol name
     , EncodeRepArgs a
     ) => EncodeAeson (Rep.Constructor name a) where
  encodeAeson (Options { sumEncoding: TaggedObject r }) (Rep.Constructor a) =
    let o :: JObject
        o = SM.insert r.tagFieldName (fromString (reflectSymbol (SProxy :: SProxy name))) SM.empty
    in fromObject case encodeRepArgs a of
          Rec o' -> o `SM.union` o'
          Arg js
            | length js == 0
            -> o
            | length js == 1
            -> SM.insert r.contentsFieldName (unsafePartial fromJust $ head js) o
            | otherwise
            -> SM.insert r.contentsFieldName (fromArray js) o

class EncodeRepArgs r where
  encodeRepArgs :: r -> RepArgsEncoding

instance encodeRepArgsNoArguments :: EncodeRepArgs Rep.NoArguments where
  encodeRepArgs Rep.NoArguments = Arg []

instance encodeRepArgsProduct :: (EncodeRepArgs a, EncodeRepArgs b) => EncodeRepArgs (Rep.Product a b) where
  encodeRepArgs (Rep.Product a b) = encodeRepArgs a <> encodeRepArgs b

instance encodeRepArgsArgument :: (EncodeJson a) => EncodeRepArgs (Rep.Argument a) where
  encodeRepArgs (Rep.Argument a) = Arg [encodeJson a]

-- | Encode any `Generic` data structure into `Json` using `Aeson` encoding
-- | (with `allNullaryToStringTag` set to `False`)
genericEncodeAeson :: forall a r. Rep.Generic a r => EncodeAeson r => Options -> a -> Json
genericEncodeAeson o = encodeAeson o <<< Rep.from
