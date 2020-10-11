module Test.Main where

import Effect (Effect)
import Data.Argonaut (encodeJson)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options (Options(..), SumEncoding(..))
import Data.Argonaut.Core (Json, fromObject, toObject, stringify)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Foreign.Object as FO
import Data.Tuple.Nested ((/\))
import Prelude (class Eq, class Show, Unit, discard, show, ($), (<>))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

data D
  = Nullary
  | Unary Int
  | Binary Int Int
  | Rec { x :: Int, y :: Int }

derive instance eqD :: Eq D
derive instance gD :: Generic D _

instance showD :: Show D where
  show Nullary = "Nullary"
  show (Unary a) = "Unary " <> show a
  show (Binary a b) = "Binary " <> show a <> " " <> show b
  show (Rec a) = "Rec { x: " <> show a.x <> ", y: " <> show a.y <> "}"

newtype SC = SC { x :: Int, y :: Int }

derive instance eqSC :: Eq SC
derive instance gSC :: Generic SC _

instance showSC :: Show SC where
  show (SC a) = "SC { x: " <> show a.x <> ", y: " <> show a.y <> " }"

opts :: Options
opts = Options
  { sumEncoding: TaggedObject { tagFieldName: "TAG", contentsFieldName: "CONTENTS" }, tagSingleConstructors: false }

optsWithTagSingleConstructors :: Options
optsWithTagSingleConstructors = Options
  { sumEncoding: TaggedObject { tagFieldName: "TAG", contentsFieldName: "CONTENTS" }, tagSingleConstructors: true }

newtype ShowJson = ShowJson Json
instance showJson :: Show ShowJson where
  show (ShowJson json) = stringify json

main :: Effect Unit
main = runTest do
  suite "Aeson encoding" do
    test "Nullary" do
      let o = genericEncodeAeson opts Nullary
      Assert.equal' (show $ ShowJson o) (Just $ FO.fromFoldable ["TAG" /\ encodeJson "Nullary"]) $ toObject o
    test "Unary" do
      let o = genericEncodeAeson opts (Unary 1)
      Assert.equal' (show $ ShowJson o) (Just $ FO.fromFoldable ["TAG" /\ encodeJson "Unary", "CONTENTS" /\ encodeJson 1]) $ toObject o
    test "Binary" do
      let o = genericEncodeAeson opts (Binary 1 2)
      Assert.equal' (show $ ShowJson o) (Just $ FO.fromFoldable ["TAG" /\ encodeJson "Binary", "CONTENTS" /\ encodeJson [1, 2]]) $ toObject o
    test "Record" do
      let o = genericEncodeAeson opts (Rec {x: 1, y: 2})
      Assert.equal' (show $ ShowJson o) (Just $ FO.fromFoldable ["TAG" /\ encodeJson "Rec", "x" /\ encodeJson 1, "y" /\ encodeJson 2]) $ toObject o
    test "Single constructor" do
      let o = genericEncodeAeson opts (SC {x: 1, y: 2})
      Assert.equal' (show $ ShowJson o) (Just $ FO.fromFoldable ["x" /\ encodeJson 1, "y" /\ encodeJson 2]) $ toObject o
    test "Tagged single constructor" do
      let o = genericEncodeAeson (optsWithTagSingleConstructors) (SC {x: 1, y: 2})
          o' = Just $ FO.fromFoldable ["TAG" /\ encodeJson "SC", "x" /\ encodeJson 1, "y" /\ encodeJson 2]
      Assert.equal' (show $ ShowJson o) o' $ toObject o

  suite "Decode" do
    test "Nullary" do
      let o = genericEncodeAeson opts Nullary
      Assert.equal (Right Nullary) (genericDecodeAeson opts o)
    test "Unary" do
      let o = genericEncodeAeson opts (Unary 1)
      Assert.equal (Right (Unary 1)) (genericDecodeAeson opts o)
    test "Binary" do
      let o = genericEncodeAeson opts (Binary 1 2)
      Assert.equal (Right (Binary 1 2)) (genericDecodeAeson opts o)
    test "Record" do
      let o = genericEncodeAeson opts (Rec {x: 1, y: 2})
      Assert.equal (Right (Rec {x: 1, y: 2})) (genericDecodeAeson opts o)
    test "Single constructor" do
      let o = fromObject $ FO.fromFoldable ["x" /\ encodeJson 1, "y" /\ encodeJson 2]
      Assert.equal (Right (SC {x: 1, y: 2})) (genericDecodeAeson opts o)
    test "Tagged single constructor" do
      let o = fromObject $ FO.fromFoldable ["TAG" /\ encodeJson "SC", "x" /\ encodeJson 1, "y" /\ encodeJson 2]
      Assert.equal (Right (SC {x: 1, y: 2})) (genericDecodeAeson optsWithTagSingleConstructors o)
