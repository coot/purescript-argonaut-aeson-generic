module Test.Main where

import Effect (Effect)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson, class DecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson, class EncodeAeson)
import Data.Argonaut.Aeson.Options (Options(..), SumEncoding(..), defaultOptions)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
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

data Nested a = Nested a
derive instance generic_Nested :: Generic (Nested a) _
instance show_Nested :: Show a => Show (Nested a) where show = genericShow
derive instance eq_Nested :: Eq a => Eq (Nested a)
instance decodeJsonNested :: DecodeJson a => DecodeJson (Nested a) where
  decodeJson a = genericDecodeAeson defaultOptions a
instance encodeJsonNested :: EncodeJson a => EncodeJson (Nested a) where
  encodeJson a = genericEncodeAeson defaultOptions a

data Siblings a b = Siblings a b
derive instance generic_Siblings :: Generic (Siblings a b) _
instance show_Siblings :: (Show a, Show b) => Show (Siblings a b) where show = genericShow
derive instance eq_Siblings :: (Eq a, Eq b) => Eq (Siblings a b)
instance decodeJsonSiblings :: (DecodeJson a, DecodeJson b) => DecodeJson (Siblings a b) where
  decodeJson a = genericDecodeAeson defaultOptions a
instance encodeJsonSiblings :: (EncodeJson a, EncodeJson b) => EncodeJson (Siblings a b) where
  encodeJson a = genericEncodeAeson defaultOptions a

data Trinity a b c = Trinity a b c
derive instance generic_Trinity :: Generic (Trinity a b c) _
instance show_Trinity :: (Show a, Show b, Show c) => Show (Trinity a b c) where show = genericShow
derive instance eq_Trinity :: (Eq a, Eq b, Eq c) => Eq (Trinity a b c)
instance decodeJsonTrinity :: (DecodeJson a, DecodeJson b, DecodeJson c) => DecodeJson (Trinity a b c) where
  decodeJson a = genericDecodeAeson defaultOptions a
instance encodeJsonTrinity :: (EncodeJson a, EncodeJson b, EncodeJson c) => EncodeJson (Trinity a b c) where
  encodeJson a = genericEncodeAeson defaultOptions a

data Inner = Inner
derive instance generic_Inner :: Generic Inner _
instance show_Inner :: Show Inner where show = genericShow
derive instance eq_Inner :: Eq Inner
instance decodeJsonInner :: DecodeJson Inner where
  decodeJson a = genericDecodeAeson defaultOptions a
instance encodeJsonInner :: EncodeJson Inner where
  encodeJson a = genericEncodeAeson defaultOptions a

data InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag = InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag
derive instance generic_InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag :: Generic InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag _
instance show_InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag :: Show InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag where show = genericShow
derive instance eq_InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag :: Eq InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag
instance decodeJsonInnerWithTagSingleConstructorsAndNoAllNullaryToStringTag :: DecodeJson InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag where
  decodeJson a = genericDecodeAeson defaultOptionsWithTagSingleConstructorsAndNoAllNullaryToStringTag a
instance encodeJsonInnerWithTagSingleConstructorsAndNoAllNullaryToStringTag :: EncodeJson InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag where
  encodeJson a = genericEncodeAeson defaultOptionsWithTagSingleConstructorsAndNoAllNullaryToStringTag a

data InnerWithNoAllNullaryToStringTag = InnerWithNoAllNullaryToStringTag
derive instance generic_InnerWithNoAllNullaryToStringTag :: Generic InnerWithNoAllNullaryToStringTag _
instance show_InnerWithNoAllNullaryToStringTag :: Show InnerWithNoAllNullaryToStringTag where show = genericShow
derive instance eq_InnerWithNoAllNullaryToStringTag :: Eq InnerWithNoAllNullaryToStringTag
instance decodeJsonInnerWithNoAllNullaryToStringTag :: DecodeJson InnerWithNoAllNullaryToStringTag where
  decodeJson a = genericDecodeAeson defaultOptionsWithNoAllNullaryToStringTag a
instance encodeJsonInnerWithNoAllNullaryToStringTag :: EncodeJson InnerWithNoAllNullaryToStringTag where
  encodeJson a = genericEncodeAeson defaultOptionsWithNoAllNullaryToStringTag a

data InnerWithTagSingleConstructors = InnerWithTagSingleConstructors
derive instance generic_InnerWithTagSingleConstructors :: Generic InnerWithTagSingleConstructors _
instance show_InnerWithTagSingleConstructors :: Show InnerWithTagSingleConstructors where show = genericShow
derive instance eq_InnerWithTagSingleConstructors :: Eq InnerWithTagSingleConstructors
instance decodeJsonInnerWithTagSingleConstructors :: DecodeJson InnerWithTagSingleConstructors where
  decodeJson a = genericDecodeAeson defaultOptionsWithTagSingleConstructors a
instance encodeJsonInnerWithTagSingleConstructors :: EncodeJson InnerWithTagSingleConstructors where
  encodeJson a = genericEncodeAeson defaultOptionsWithTagSingleConstructors a

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
      , checkAesonCompatibility (Nested Inner) "[]"
      , checkAesonCompatibility (Nested ([ ] :: Array Inner)) "[]"
      , checkAesonCompatibility (Nested [Inner]) "[[]]"
      , checkAesonCompatibility (Nested [Inner,Inner]) "[[],[]]"
      , checkAesonCompatibility (Nested InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag) "{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}"
      , checkAesonCompatibility (Nested ([ ] :: Array InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag)) "[]"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag]) "[{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}]"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag,InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag]) "[{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"},{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}]"
      , checkAesonCompatibility (Nested InnerWithNoAllNullaryToStringTag) "[]"
      , checkAesonCompatibility (Nested ([ ] :: Array InnerWithNoAllNullaryToStringTag)) "[]"
      , checkAesonCompatibility (Nested [InnerWithNoAllNullaryToStringTag]) "[[]]"
      , checkAesonCompatibility (Nested [InnerWithNoAllNullaryToStringTag,InnerWithNoAllNullaryToStringTag]) "[[],[]]"
      , checkAesonCompatibility (Nested InnerWithTagSingleConstructors) "\"InnerWithTagSingleConstructors\""
      , checkAesonCompatibility (Nested ([ ] :: Array InnerWithTagSingleConstructors)) "[]"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructors]) "[\"InnerWithTagSingleConstructors\"]"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructors,InnerWithTagSingleConstructors]) "[\"InnerWithTagSingleConstructors\",\"InnerWithTagSingleConstructors\"]"
      , checkAesonCompatibility (Siblings (Nested Inner) InnerWithNoAllNullaryToStringTag) "[[],[]]"
      , checkAesonCompatibility (Siblings (Nested (Siblings (Nested InnerWithTagSingleConstructors) "meow")) 3.1415927) "[[\"InnerWithTagSingleConstructors\",\"meow\"],3.1415927]"
      , checkAesonCompatibility (Trinity 3.1415927 (Nested (Nested (Siblings InnerWithNoAllNullaryToStringTag InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag))) ["oink","waff","quack"]) "[3.1415927,[[],{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}],[\"oink\",\"waff\",\"quack\"]]"
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
      , checkAesonCompatibility (Nested Inner) "[]"
      , checkAesonCompatibility (Nested ([ ] :: Array Inner)) "[]"
      , checkAesonCompatibility (Nested [Inner]) "[[]]"
      , checkAesonCompatibility (Nested [Inner,Inner]) "[[],[]]"
      , checkAesonCompatibility (Nested InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag) "{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}"
      , checkAesonCompatibility (Nested ([ ] :: Array InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag)) "[]"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag]) "[{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}]"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag,InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag]) "[{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"},{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}]"
      , checkAesonCompatibility (Nested InnerWithNoAllNullaryToStringTag) "[]"
      , checkAesonCompatibility (Nested ([ ] :: Array InnerWithNoAllNullaryToStringTag)) "[]"
      , checkAesonCompatibility (Nested [InnerWithNoAllNullaryToStringTag]) "[[]]"
      , checkAesonCompatibility (Nested [InnerWithNoAllNullaryToStringTag,InnerWithNoAllNullaryToStringTag]) "[[],[]]"
      , checkAesonCompatibility (Nested InnerWithTagSingleConstructors) "\"InnerWithTagSingleConstructors\""
      , checkAesonCompatibility (Nested ([ ] :: Array InnerWithTagSingleConstructors)) "[]"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructors]) "[\"InnerWithTagSingleConstructors\"]"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructors,InnerWithTagSingleConstructors]) "[\"InnerWithTagSingleConstructors\",\"InnerWithTagSingleConstructors\"]"
      , checkAesonCompatibility (Siblings (Nested Inner) InnerWithNoAllNullaryToStringTag) "[[],[]]"
      , checkAesonCompatibility (Siblings (Nested (Siblings (Nested InnerWithTagSingleConstructors) "meow")) 3.1415927) "[[\"InnerWithTagSingleConstructors\",\"meow\"],3.1415927]"
      , checkAesonCompatibility (Trinity 3.1415927 (Nested (Nested (Siblings InnerWithNoAllNullaryToStringTag InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag))) ["oink","waff","quack"]) "[3.1415927,[[],{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}],[\"oink\",\"waff\",\"quack\"]]"
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
      , checkAesonCompatibility (Nested Inner) "{\"tag\":\"Nested\",\"contents\":[]}"
      , checkAesonCompatibility (Nested ([ ] :: Array Inner)) "{\"tag\":\"Nested\",\"contents\":[]}"
      , checkAesonCompatibility (Nested [Inner]) "{\"tag\":\"Nested\",\"contents\":[[]]}"
      , checkAesonCompatibility (Nested [Inner,Inner]) "{\"tag\":\"Nested\",\"contents\":[[],[]]}"
      , checkAesonCompatibility (Nested InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag) "{\"tag\":\"Nested\",\"contents\":{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}}"
      , checkAesonCompatibility (Nested ([ ] :: Array InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag)) "{\"tag\":\"Nested\",\"contents\":[]}"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag]) "{\"tag\":\"Nested\",\"contents\":[{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}]}"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag,InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag]) "{\"tag\":\"Nested\",\"contents\":[{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"},{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}]}"
      , checkAesonCompatibility (Nested InnerWithNoAllNullaryToStringTag) "{\"tag\":\"Nested\",\"contents\":[]}"
      , checkAesonCompatibility (Nested ([ ] :: Array InnerWithNoAllNullaryToStringTag)) "{\"tag\":\"Nested\",\"contents\":[]}"
      , checkAesonCompatibility (Nested [InnerWithNoAllNullaryToStringTag]) "{\"tag\":\"Nested\",\"contents\":[[]]}"
      , checkAesonCompatibility (Nested [InnerWithNoAllNullaryToStringTag,InnerWithNoAllNullaryToStringTag]) "{\"tag\":\"Nested\",\"contents\":[[],[]]}"
      , checkAesonCompatibility (Nested InnerWithTagSingleConstructors) "{\"tag\":\"Nested\",\"contents\":\"InnerWithTagSingleConstructors\"}"
      , checkAesonCompatibility (Nested ([ ] :: Array InnerWithTagSingleConstructors)) "{\"tag\":\"Nested\",\"contents\":[]}"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructors]) "{\"tag\":\"Nested\",\"contents\":[\"InnerWithTagSingleConstructors\"]}"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructors,InnerWithTagSingleConstructors]) "{\"tag\":\"Nested\",\"contents\":[\"InnerWithTagSingleConstructors\",\"InnerWithTagSingleConstructors\"]}"
      , checkAesonCompatibility (Siblings (Nested Inner) InnerWithNoAllNullaryToStringTag) "{\"tag\":\"Siblings\",\"contents\":[[],[]]}"
      , checkAesonCompatibility (Siblings (Nested (Siblings (Nested InnerWithTagSingleConstructors) "meow")) 3.1415927) "{\"tag\":\"Siblings\",\"contents\":[[\"InnerWithTagSingleConstructors\",\"meow\"],3.1415927]}"
      , checkAesonCompatibility (Trinity 3.1415927 (Nested (Nested (Siblings InnerWithNoAllNullaryToStringTag InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag))) ["oink","waff","quack"]) "{\"tag\":\"Trinity\",\"contents\":[3.1415927,[[],{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}],[\"oink\",\"waff\",\"quack\"]]}"
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
      , checkAesonCompatibility (Nested Inner) "{\"tag\":\"Nested\",\"contents\":[]}"
      , checkAesonCompatibility (Nested ([ ] :: Array Inner)) "{\"tag\":\"Nested\",\"contents\":[]}"
      , checkAesonCompatibility (Nested [Inner]) "{\"tag\":\"Nested\",\"contents\":[[]]}"
      , checkAesonCompatibility (Nested [Inner,Inner]) "{\"tag\":\"Nested\",\"contents\":[[],[]]}"
      , checkAesonCompatibility (Nested InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag) "{\"tag\":\"Nested\",\"contents\":{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}}"
      , checkAesonCompatibility (Nested ([ ] :: Array InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag)) "{\"tag\":\"Nested\",\"contents\":[]}"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag]) "{\"tag\":\"Nested\",\"contents\":[{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}]}"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag,InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag]) "{\"tag\":\"Nested\",\"contents\":[{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"},{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}]}"
      , checkAesonCompatibility (Nested InnerWithNoAllNullaryToStringTag) "{\"tag\":\"Nested\",\"contents\":[]}"
      , checkAesonCompatibility (Nested ([ ] :: Array InnerWithNoAllNullaryToStringTag)) "{\"tag\":\"Nested\",\"contents\":[]}"
      , checkAesonCompatibility (Nested [InnerWithNoAllNullaryToStringTag]) "{\"tag\":\"Nested\",\"contents\":[[]]}"
      , checkAesonCompatibility (Nested [InnerWithNoAllNullaryToStringTag,InnerWithNoAllNullaryToStringTag]) "{\"tag\":\"Nested\",\"contents\":[[],[]]}"
      , checkAesonCompatibility (Nested InnerWithTagSingleConstructors) "{\"tag\":\"Nested\",\"contents\":\"InnerWithTagSingleConstructors\"}"
      , checkAesonCompatibility (Nested ([ ] :: Array InnerWithTagSingleConstructors)) "{\"tag\":\"Nested\",\"contents\":[]}"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructors]) "{\"tag\":\"Nested\",\"contents\":[\"InnerWithTagSingleConstructors\"]}"
      , checkAesonCompatibility (Nested [InnerWithTagSingleConstructors,InnerWithTagSingleConstructors]) "{\"tag\":\"Nested\",\"contents\":[\"InnerWithTagSingleConstructors\",\"InnerWithTagSingleConstructors\"]}"
      , checkAesonCompatibility (Siblings (Nested Inner) InnerWithNoAllNullaryToStringTag) "{\"tag\":\"Siblings\",\"contents\":[[],[]]}"
      , checkAesonCompatibility (Siblings (Nested (Siblings (Nested InnerWithTagSingleConstructors) "meow")) 3.1415927) "{\"tag\":\"Siblings\",\"contents\":[[\"InnerWithTagSingleConstructors\",\"meow\"],3.1415927]}"
      , checkAesonCompatibility (Trinity 3.1415927 (Nested (Nested (Siblings InnerWithNoAllNullaryToStringTag InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag))) ["oink","waff","quack"]) "{\"tag\":\"Trinity\",\"contents\":[3.1415927,[[],{\"tag\":\"InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag\"}],[\"oink\",\"waff\",\"quack\"]]}"
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
          , checkInvertibility Inner
          , checkInvertibility (Nested Inner)
          , checkInvertibility (Nested ([ ] :: Array Inner))
          , checkInvertibility (Nested [Inner])
          , checkInvertibility (Nested [Inner, Inner])
          , checkInvertibility (Nested InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag)
          , checkInvertibility (Nested ([ ] :: Array InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag))
          , checkInvertibility (Nested [InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag])
          , checkInvertibility (Nested [InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag, InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag])
          , checkInvertibility (Nested InnerWithNoAllNullaryToStringTag)
          , checkInvertibility (Nested ([ ] :: Array InnerWithNoAllNullaryToStringTag))
          , checkInvertibility (Nested [InnerWithNoAllNullaryToStringTag])
          , checkInvertibility (Nested [InnerWithNoAllNullaryToStringTag, InnerWithNoAllNullaryToStringTag])
          , checkInvertibility (Nested InnerWithTagSingleConstructors)
          , checkInvertibility (Nested ([ ] :: Array InnerWithTagSingleConstructors))
          , checkInvertibility (Nested [InnerWithTagSingleConstructors])
          , checkInvertibility (Nested [InnerWithTagSingleConstructors, InnerWithTagSingleConstructors])
          , checkInvertibility (Siblings (Nested Inner) InnerWithNoAllNullaryToStringTag)
          , checkInvertibility (Siblings (Nested (Siblings (Nested InnerWithTagSingleConstructors) "meow")) 3.1415927)
          , checkInvertibility (Trinity 3.1415927 (Nested (Nested (Siblings InnerWithNoAllNullaryToStringTag InnerWithTagSingleConstructorsAndNoAllNullaryToStringTag))) ["oink","waff","quack"])
          ]
        options =
          [ defaultOptions
          , defaultOptionsWithNoAllNullaryToStringTag
          , defaultOptionsWithTagSingleConstructors
          , defaultOptionsWithTagSingleConstructorsAndNoAllNullaryToStringTag
          ]
    in do traverse_ (_ $ examples) (map checkManyWithOptions options)
