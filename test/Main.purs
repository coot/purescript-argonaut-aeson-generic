module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (encodeJson, toObject, toString)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options (class IsAllNullary, Options(..), SumEncoding(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, from)
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Tuple.Nested ((/\))
import Prelude (class Eq, class Show, Unit, discard, show, ($), (<<<), (<>))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

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

unsafeLog :: forall a e. a -> Eff (console :: CONSOLE | e) Unit
unsafeLog = log <<< unsafeCoerce

opts :: Options
opts = Options { sumEncoding: TaggedObject { tagFieldName: "TAG", contentsFieldName: "CONTENTS" } }

main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) Unit 
main = runTest do
  suite "Aeson encoding" do
    test "Nullary" do
      let o = toObject $ genericEncodeAeson opts Nullary
      Assert.equal (Just $ SM.fromFoldable ["TAG" /\ encodeJson "Nullary"]) o
    test "Unary" do
      let o = toObject $ genericEncodeAeson opts (Unary 1)
      Assert.equal (Just $ SM.fromFoldable ["TAG" /\ encodeJson "Unary", "CONTENTS" /\ encodeJson 1]) o
    test "Binary" do
      let o = toObject $ genericEncodeAeson opts (Binary 1 2)
      Assert.equal (Just $ SM.fromFoldable ["TAG" /\ encodeJson "Binary", "CONTENTS" /\ encodeJson [1, 2]]) o
    test "Record" do
      let o = toObject $ genericEncodeAeson opts (Rec {x: 1, y: 2})
      Assert.equal (Just $ SM.fromFoldable ["TAG" /\ encodeJson "Rec", "x" /\ encodeJson 1, "y" /\ encodeJson 2]) o

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
