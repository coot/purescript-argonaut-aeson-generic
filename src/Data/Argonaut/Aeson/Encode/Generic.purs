module Data.Argonaut.Aeson.Encode.Generic
  ( class EncodeAeson
  , class EncodeAeson'
  , class EncodeRepArgs
  , RepArgsEncoding(..)
  , class EncodeRepFields
  , encodeFields
  , encodeAeson
  , encodeAeson'
  , encodeRepArgs
  , genericEncodeAeson
  ) where

import Prelude (class Semigroup, otherwise, ($), (<<<), (<>), (==))

import Record (get)
import Data.Argonaut.Aeson.Options (Options(Options), SumEncoding(..))
import Data.Argonaut.Aeson.Helpers (class AreAllConstructorsNullary, class IsSingleConstructor, Mode(..), areAllConstructorsNullary, isSingleConstructor)
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString, jsonEmptyArray)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Array (cons, uncons, head, length, snoc)
import Data.Generic.Rep as Rep
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Object as FO
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))
import Type.Row (class Cons)
import Type.RowList (class RowToList, Nil, Cons, RLProxy(..), kind RowList)
    
class EncodeAeson r where
  encodeAeson :: Options -> r -> Json

instance encodeAesonInt :: EncodeAeson' Int => EncodeAeson Int where
  encodeAeson = encodeAeson' mode
    where
      mode = Mode
        { _Mode_ConstructorIsSingle: false
        , _Mode_ConstructorsAreAllNullary: false
        }

instance encodeAesonNoConstructors :: EncodeAeson' Rep.NoConstructors => EncodeAeson Rep.NoConstructors where
  encodeAeson = encodeAeson' mode
    where
      mode = Mode
        { _Mode_ConstructorIsSingle: false
        , _Mode_ConstructorsAreAllNullary: false
        }

instance encodeAesonConstructor
  :: ( EncodeRepArgs a
     , IsSymbol name
     , AreAllConstructorsNullary (Rep.Constructor name a)
     , IsSingleConstructor (Rep.Constructor name a)
     )
  => EncodeAeson (Rep.Constructor name a) where
  encodeAeson o thing = encodeAeson' mode o thing
    where
      mode = Mode
        { _Mode_ConstructorIsSingle: isSingleConstructor (Proxy :: Proxy (Rep.Constructor name a))
        , _Mode_ConstructorsAreAllNullary: areAllConstructorsNullary (Proxy :: Proxy (Rep.Constructor name a))
        }

instance encodeAesonSum
  :: ( EncodeAeson' (Rep.Sum a b)
     , AreAllConstructorsNullary (Rep.Sum a b)
     , IsSingleConstructor (Rep.Sum a b)
     )
  => EncodeAeson (Rep.Sum a b) where
  encodeAeson o thing = encodeAeson' mode o thing
    where
      mode = Mode
        { _Mode_ConstructorIsSingle: isSingleConstructor (Proxy :: Proxy (Rep.Sum a b))
        , _Mode_ConstructorsAreAllNullary: areAllConstructorsNullary (Proxy :: Proxy (Rep.Sum a b))
        }

class EncodeAeson' r where
  encodeAeson' :: Mode -> Options -> r -> Json

instance encodeAesonInt' :: EncodeAeson' Int where
  encodeAeson' _ _ = encodeJson

instance encodeAesonNoConstructors' :: EncodeAeson' Rep.NoConstructors where
  encodeAeson' x = encodeAeson' x

instance encodeAesonSum' :: (EncodeAeson' a, EncodeAeson' b) => EncodeAeson' (Rep.Sum a b) where
  encodeAeson' o mode (Rep.Inl a) = encodeAeson' o mode a
  encodeAeson' o mode (Rep.Inr b) = encodeAeson' o mode b

data RepArgsEncoding
  = Arg (Array Json)
  | Rec (FO.Object Json)

instance semigroupRepArgsEncoding :: Semigroup RepArgsEncoding where
  append (Arg a) (Arg b) = Arg (a <> b)
  append (Arg a) (Rec b) = Arg (snoc a $ fromObject b)
  append (Rec a) (Arg b) = Arg (cons (fromObject a) b)
  append (Rec a) (Rec b) = Arg [fromObject a, fromObject b]

instance encodeAesonConstructor' :: (IsSymbol name, EncodeRepArgs a) => EncodeAeson' (Rep.Constructor name a) where
  encodeAeson' mode options (Rep.Constructor arguments)  =
    let name = reflectSymbol (SProxy :: SProxy name)
    in case {mode: mode, options: options} of

      { mode: Mode {_Mode_ConstructorIsSingle: true, _Mode_ConstructorsAreAllNullary: true}
      , options: Options {tagSingleConstructors: false, allNullaryToStringTag: true}
      } -> jsonEmptyArray

      { mode: Mode {_Mode_ConstructorsAreAllNullary: true}
      , options: Options {allNullaryToStringTag: true}
      } -> encodeJson name

      { mode: Mode {_Mode_ConstructorIsSingle: true}
      , options: Options {tagSingleConstructors: false}
      } -> case encodeRepArgs arguments of
        Rec foreignObject -> fromObject foreignObject
        Arg xs -> case uncons xs of
          Nothing -> jsonEmptyArray
          Just {head: x, tail: ys} -> case uncons ys of
            Nothing -> x
            Just {head: y, tail: zs} -> fromArray ([x, y] <> zs)

      {options: Options {sumEncoding: TaggedObject taggedObject}} ->
        let o :: FO.Object Json
            o = FO.insert taggedObject.tagFieldName (fromString (reflectSymbol (SProxy :: SProxy name))) FO.empty
        in fromObject case encodeRepArgs arguments of
              Rec o' -> o `FO.union` o'
              Arg js
                | length js == 0
                -> o
                | length js == 1
                -> FO.insert taggedObject.contentsFieldName (unsafePartial fromJust $ head js) o
                | otherwise
                -> FO.insert taggedObject.contentsFieldName (fromArray js) o

class EncodeRepArgs r where
  encodeRepArgs :: r -> RepArgsEncoding

instance encodeRepArgsNoArguments :: EncodeRepArgs Rep.NoArguments where
  encodeRepArgs Rep.NoArguments = Arg []

instance encodeRepArgsProduct :: (EncodeRepArgs a, EncodeRepArgs b) => EncodeRepArgs (Rep.Product a b) where
  encodeRepArgs (Rep.Product a b) = encodeRepArgs a <> encodeRepArgs b

instance encodeRepArgsRec :: (RowToList r rs, EncodeRepFields rs r) ⇒ EncodeRepArgs (Rep.Argument (Record r)) where
  encodeRepArgs ( Rep.Argument fields ) =
    Rec $ encodeFields (RLProxy ∷ RLProxy rs) fields

else 

instance encodeRepAesonArgsArgument :: EncodeJson a => EncodeRepArgs (Rep.Argument a) where
  encodeRepArgs (Rep.Argument a) = Arg [encodeJson a]


-- | Encode record fields 
class EncodeRepFields (rs :: RowList Type) (row :: # Type) | rs -> row where
  encodeFields :: RLProxy rs -> Record row -> (FO.Object Json)

instance encodeRepFieldsCons ∷ ( IsSymbol name
                               , EncodeJson ty 
                               , Cons name ty tailRow row
                               , EncodeRepFields tail row) ⇒ EncodeRepFields (Cons name ty tail) row where
  encodeFields _ r = 
    let
      name = reflectSymbol (SProxy ∷ SProxy name)
      value = get (SProxy ∷ SProxy name) r

      rest ∷ FO.Object Json
      rest = encodeFields (RLProxy ∷ RLProxy tail) r
    in
     FO.insert name (encodeJson value) rest

instance encodeRepFieldsNil ∷ EncodeRepFields Nil row where
  encodeFields _ _ = FO.empty
  
-- | Encode any `Generic` data structure into `Json` using `Aeson` encoding
genericEncodeAeson :: forall a r. Rep.Generic a r => EncodeAeson r => Options -> a -> Json
genericEncodeAeson o = encodeAeson o <<< Rep.from
