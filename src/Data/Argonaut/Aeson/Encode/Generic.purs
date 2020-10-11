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

import Prelude

import Record (get)
import Data.Argonaut.Aeson.Options (Options(Options), SumEncoding(..))
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Array (cons, head, length, snoc)
import Data.Generic.Rep as Rep
import Data.Maybe (fromJust)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Object as FO
import Partial.Unsafe (unsafePartial)
import Type.Row (class Cons)
import Type.RowList (class RowToList, Nil, Cons, RLProxy(..), kind RowList)
    
class EncodeAeson r where
  encodeAeson :: Options -> r -> Json

instance encodeAesonSingleConstructor :: (EncodeRepArgs a, IsSymbol name) => EncodeAeson (Rep.Constructor name a) where
  encodeAeson (Options options) (Rep.Constructor a) =
    if options.tagSingleConstructors
    then (encodeAeson' (Options options) :: (Rep.Constructor name a -> Json)) (Rep.Constructor a)
    else fromObject case encodeRepArgs a of
      Rec o -> o
      Arg _ -> FO.empty  -- Not implemented.
else
instance encodeAesonAny :: EncodeAeson' r => EncodeAeson r where
  encodeAeson = encodeAeson'

class EncodeAeson' r where
  encodeAeson' :: Options -> r -> Json


instance encodeAesonInt :: EncodeAeson' Int where
  encodeAeson' _ = encodeJson

instance encodeAesonNoConstructors :: EncodeAeson' Rep.NoConstructors where
  encodeAeson' o r = encodeAeson' o r

instance encodeAesonSum :: (EncodeAeson' a, EncodeAeson' b) => EncodeAeson' (Rep.Sum a b) where
  encodeAeson' o (Rep.Inl a) = encodeAeson' o a
  encodeAeson' o (Rep.Inr b) = encodeAeson' o b

data RepArgsEncoding
  = Arg (Array Json)
  | Rec (FO.Object Json)

instance semigroupRepArgsEncoding :: Semigroup RepArgsEncoding where
  append (Arg a) (Arg b) = Arg (a <> b)
  append (Arg a) (Rec b) = Arg (snoc a $ fromObject b)
  append (Rec a) (Arg b) = Arg (cons (fromObject a) b)
  append (Rec a) (Rec b) = Arg [fromObject a, fromObject b]

instance encodeAesonConstructor :: (IsSymbol name, EncodeRepArgs a) => EncodeAeson' (Rep.Constructor name a) where
  encodeAeson' (Options { sumEncoding: TaggedObject r }) (Rep.Constructor a) =
    let o :: FO.Object Json
        o = FO.insert r.tagFieldName (fromString (reflectSymbol (SProxy :: SProxy name))) FO.empty
    in fromObject case encodeRepArgs a of
          Rec o' -> o `FO.union` o'
          Arg js
            | length js == 0
            -> o
            | length js == 1
            -> FO.insert r.contentsFieldName (unsafePartial fromJust $ head js) o
            | otherwise
            -> FO.insert r.contentsFieldName (fromArray js) o

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
class EncodeRepFields (rs :: RowList) (row :: # Type) | rs -> row where
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
-- | (with `allNullaryToStringTag` set to `False`)
genericEncodeAeson :: forall a r. Rep.Generic a r => EncodeAeson r => Options -> a -> Json
genericEncodeAeson o = encodeAeson o <<< Rep.from
