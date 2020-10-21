module Test.Main where

import Effect (Effect)
import Data.Argonaut (encodeJson)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson, class DecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options (Options(..), SumEncoding(..), defaultOptions)
import Data.Argonaut.Core (Json, fromObject, toObject, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Foreign.Object as FO
import Data.Tuple.Nested ((/\))
import Prelude (class Eq, class Show, Unit, discard, show, map, ($), (<>), (+), (<<<), (<=<))
import Test.Unit (suite, test, TestSuite)
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
  { sumEncoding: TaggedObject { tagFieldName: "TAG", contentsFieldName: "CONTENTS" }
  , tagSingleConstructors: false
  , allNullaryToStringTag: true
  }

optsWithTagSingleConstructors :: Options
optsWithTagSingleConstructors = Options
  { sumEncoding: TaggedObject { tagFieldName: "TAG", contentsFieldName: "CONTENTS" }
  , tagSingleConstructors: true
  , allNullaryToStringTag: true
  }

newtype ShowJson = ShowJson Json
instance showJson :: Show ShowJson where
  show (ShowJson json) = stringify json

defaultOptionsWithTagSingleConstructorsAndNoAllNullaryToStringTag :: Options
defaultOptionsWithTagSingleConstructorsAndNoAllNullaryToStringTag = Options
  { sumEncoding: TaggedObject { tagFieldName: "tag", contentsFieldName: "contents" }
  , tagSingleConstructors: true
  , allNullaryToStringTag: false
  }

defaultOptionsWithNoAllNullaryToStringTag :: Options
defaultOptionsWithNoAllNullaryToStringTag = Options
  { sumEncoding: TaggedObject { tagFieldName: "tag", contentsFieldName: "contents" }
  , tagSingleConstructors: false
  , allNullaryToStringTag: false
  }

defaultOptionsWithTagSingleConstructors :: Options
defaultOptionsWithTagSingleConstructors = Options
  { sumEncoding: TaggedObject { tagFieldName: "tag", contentsFieldName: "contents" }
  , tagSingleConstructors: true
  , allNullaryToStringTag: true
  }

checkAesonCompatibility :: forall a rep. Eq a => Show a => Generic a rep => DecodeAeson rep => a -> String -> Options -> TestSuite
checkAesonCompatibility value canonicalEncoding options =
  test (show value <> " as " <> canonicalEncoding) do
    let hopefullyDecodedValue = (genericDecodeAeson options <=< jsonParser) canonicalEncoding
    Assert.equal (Right value) hopefullyDecodedValue

checkManyWithOptions :: Options -> Array (Options -> TestSuite) -> TestSuite
checkManyWithOptions options testCaseConstructors = suite (show options) (traverse_ (_$ options) testCaseConstructors)

data SingleNullary = SingleNullary
derive instance generic_SingleNullary :: Generic SingleNullary _
instance show_SingleNullary :: Show SingleNullary where show = genericShow
derive instance eq_SingleNullary :: Eq SingleNullary

data SingleUnary = SingleUnary Int
derive instance generic_SingleUnary :: Generic SingleUnary _
instance show_SingleUnary :: Show SingleUnary where show = genericShow
derive instance eq_SingleUnary :: Eq SingleUnary

data SingleBinary = SingleBinary Int Int
derive instance generic_SingleBinary :: Generic SingleBinary _
instance show_SingleBinary :: Show SingleBinary where show = genericShow
derive instance eq_SingleBinary :: Eq SingleBinary

data RecordUnary = RecordUnary {recordUnaryField1 :: Int}
derive instance generic_RecordUnary :: Generic RecordUnary _
instance show_RecordUnary :: Show RecordUnary where show = genericShow
derive instance eq_RecordUnary :: Eq RecordUnary

data RecordBinary = RecordBinary {recordBinaryField1 :: Int, recordBinaryField2 :: Int}
derive instance generic_RecordBinary :: Generic RecordBinary _
instance show_RecordBinary :: Show RecordBinary where show = genericShow
derive instance eq_RecordBinary :: Eq RecordBinary

data Enumeration = Enumeration1 | Enumeration2 | Enumeration3
derive instance generic_Enumeration :: Generic Enumeration _
instance show_Enumeration :: Show Enumeration where show = genericShow
derive instance eq_Enumeration :: Eq Enumeration

data Variety = VarietyNullary | VarietyUnary Int | VarietyBinary Int Int
derive instance generic_Variety :: Generic Variety _
instance show_Variety :: Show Variety where show = genericShow
derive instance eq_Variety :: Eq Variety

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
  suite "Aeson compatibility" do
    checkManyWithOptions defaultOptions
      [ checkAesonCompatibility SingleNullary "[]"
      , checkAesonCompatibility (SingleUnary 1) "1"
      , checkAesonCompatibility (SingleBinary 1 2) "[1,2]"
      , checkAesonCompatibility (RecordUnary {recordUnaryField1: 1}) "{\"recordUnaryField1\":1}"
      , checkAesonCompatibility (RecordBinary {recordBinaryField1: 1, recordBinaryField2: 2}) "{\"recordBinaryField1\":1,\"recordBinaryField2\":2}"
      , checkAesonCompatibility Enumeration1 "\"Enumeration1\""
      , checkAesonCompatibility Enumeration2 "\"Enumeration2\""
      , checkAesonCompatibility Enumeration3 "\"Enumeration3\""
      , checkAesonCompatibility VarietyNullary "{\"tag\":\"VarietyNullary\"}"
      , checkAesonCompatibility (VarietyUnary 1) "{\"tag\":\"VarietyUnary\",\"contents\":1}"
      , checkAesonCompatibility (VarietyBinary 1 2) "{\"tag\":\"VarietyBinary\",\"contents\":[1,2]}"
      ]
    checkManyWithOptions defaultOptionsWithNoAllNullaryToStringTag
      [ checkAesonCompatibility SingleNullary "[]"
      , checkAesonCompatibility (SingleUnary 1) "1"
      , checkAesonCompatibility (SingleBinary 1 2) "[1,2]"
      , checkAesonCompatibility (RecordUnary {recordUnaryField1: 1}) "{\"recordUnaryField1\":1}"
      , checkAesonCompatibility (RecordBinary {recordBinaryField1: 1, recordBinaryField2: 2}) "{\"recordBinaryField1\":1,\"recordBinaryField2\":2}"
      , checkAesonCompatibility Enumeration1 "{\"tag\":\"Enumeration1\"}"
      , checkAesonCompatibility Enumeration2 "{\"tag\":\"Enumeration2\"}"
      , checkAesonCompatibility Enumeration3 "{\"tag\":\"Enumeration3\"}"
      , checkAesonCompatibility VarietyNullary "{\"tag\":\"VarietyNullary\"}"
      , checkAesonCompatibility (VarietyUnary 1) "{\"tag\":\"VarietyUnary\",\"contents\":1}"
      , checkAesonCompatibility (VarietyBinary 1 2) "{\"tag\":\"VarietyBinary\",\"contents\":[1,2]}"
      ]
    checkManyWithOptions defaultOptionsWithTagSingleConstructors
      [ checkAesonCompatibility SingleNullary "\"SingleNullary\""
      , checkAesonCompatibility (SingleUnary 1) "{\"tag\":\"SingleUnary\",\"contents\":1}"
      , checkAesonCompatibility (SingleBinary 1 2) "{\"tag\":\"SingleBinary\",\"contents\":[1,2]}"
      , checkAesonCompatibility (RecordUnary {recordUnaryField1: 1}) "{\"recordUnaryField1\":1,\"tag\":\"RecordUnary\"}"
      , checkAesonCompatibility (RecordBinary {recordBinaryField1: 1, recordBinaryField2: 2}) "{\"tag\":\"RecordBinary\",\"recordBinaryField1\":1,\"recordBinaryField2\":2}"
      , checkAesonCompatibility Enumeration1 "\"Enumeration1\""
      , checkAesonCompatibility Enumeration2 "\"Enumeration2\""
      , checkAesonCompatibility Enumeration3 "\"Enumeration3\""
      , checkAesonCompatibility VarietyNullary "{\"tag\":\"VarietyNullary\"}"
      , checkAesonCompatibility (VarietyUnary 1) "{\"tag\":\"VarietyUnary\",\"contents\":1}"
      , checkAesonCompatibility (VarietyBinary 1 2) "{\"tag\":\"VarietyBinary\",\"contents\":[1,2]}"
      ]
    checkManyWithOptions defaultOptionsWithTagSingleConstructorsAndNoAllNullaryToStringTag
      [ checkAesonCompatibility SingleNullary "{\"tag\":\"SingleNullary\"}"
      , checkAesonCompatibility (SingleUnary 1) "{\"tag\":\"SingleUnary\",\"contents\":1}"
      , checkAesonCompatibility (SingleBinary 1 2) "{\"tag\":\"SingleBinary\",\"contents\":[1,2]}"
      , checkAesonCompatibility (RecordUnary {recordUnaryField1: 1}) "{\"recordUnaryField1\":1,\"tag\":\"RecordUnary\"}"
      , checkAesonCompatibility (RecordBinary {recordBinaryField1: 1, recordBinaryField2: 2}) "{\"tag\":\"RecordBinary\",\"recordBinaryField1\":1,\"recordBinaryField2\":2}"
      , checkAesonCompatibility Enumeration1 "{\"tag\":\"Enumeration1\"}"
      , checkAesonCompatibility Enumeration2 "{\"tag\":\"Enumeration2\"}"
      , checkAesonCompatibility Enumeration3 "{\"tag\":\"Enumeration3\"}"
      , checkAesonCompatibility VarietyNullary "{\"tag\":\"VarietyNullary\"}"
      , checkAesonCompatibility (VarietyUnary 1) "{\"tag\":\"VarietyUnary\",\"contents\":1}"
      , checkAesonCompatibility (VarietyBinary 1 2) "{\"tag\":\"VarietyBinary\",\"contents\":[1,2]}"
      ]
