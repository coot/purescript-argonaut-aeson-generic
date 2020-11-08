module Test.Main where

import Effect (Effect)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson, class DecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson, class EncodeAeson)
import Data.Argonaut.Aeson.Options (Options(..), SumEncoding(..), defaultOptions)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Prelude (class Eq, class Show, Unit, discard, show, map, ($), (<>), (<<<), (<=<))
import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

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

checkInvertibility :: forall a rep. Eq a => Show a => Generic a rep => EncodeAeson rep => DecodeAeson rep => a -> Options -> TestSuite
checkInvertibility value options =
  let encoding = (stringify <<< genericEncodeAeson options) value
      hopefullyDecodedValue = (genericDecodeAeson options <=< jsonParser) encoding
  in test (show value <> " as " <> encoding) do Assert.equal (Right value) hopefullyDecodedValue

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
  suite "Invertibility"
    let examples =
          [ checkInvertibility SingleNullary
          , checkInvertibility (SingleUnary 1)
          , checkInvertibility (SingleBinary 1 2)
          , checkInvertibility (RecordUnary {recordUnaryField1: 1})
          , checkInvertibility (RecordBinary {recordBinaryField1: 1, recordBinaryField2: 2})
          , checkInvertibility Enumeration1
          , checkInvertibility Enumeration2
          , checkInvertibility Enumeration3
          , checkInvertibility VarietyNullary
          , checkInvertibility (VarietyUnary 1)
          , checkInvertibility (VarietyBinary 1 2)
          ]
        options =
          [ defaultOptions
          , defaultOptionsWithNoAllNullaryToStringTag
          , defaultOptionsWithTagSingleConstructors
          , defaultOptionsWithTagSingleConstructorsAndNoAllNullaryToStringTag
          ]
    in do traverse_ (_ $ examples) (map checkManyWithOptions options)
