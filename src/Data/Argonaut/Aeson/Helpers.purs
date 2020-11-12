module Data.Argonaut.Aeson.Helpers where

import Prelude ((&&))

import Data.Generic.Rep as Rep
import Type.Proxy (Proxy(..))

class IsSingleConstructor r where
  isSingleConstructor :: Proxy r -> Boolean

instance isSingleConstructor_NoConstructors :: IsSingleConstructor Rep.NoConstructors where
  isSingleConstructor _ = false

instance isSingleConstructor_Sum :: IsSingleConstructor (Rep.Sum a b)
  where
    isSingleConstructor _ = false

instance isSingleConstructor_Constructor :: IsSingleConstructor (Rep.Constructor name value) where
  isSingleConstructor _ = true

class AreAllConstructorsNullary r where
  areAllConstructorsNullary :: Proxy r -> Boolean

instance areAllConstructorsNullary_NoConstructors :: AreAllConstructorsNullary Rep.NoConstructors where
  areAllConstructorsNullary _ = true

instance areAllConstructorsNullary_Sum
  :: (AreAllConstructorsNullary a, AreAllConstructorsNullary b)
  => AreAllConstructorsNullary (Rep.Sum a b) where
  areAllConstructorsNullary _ = areAllConstructorsNullary (Proxy :: Proxy a) && areAllConstructorsNullary (Proxy :: Proxy b)

instance areAllConstructorsNullary_Constructor
  :: AreAllConstructorsNullary value
  => AreAllConstructorsNullary (Rep.Constructor name value) where
  areAllConstructorsNullary _ = areAllConstructorsNullary (Proxy :: Proxy value)

instance areAllConstructorsNullary_NoArguments :: AreAllConstructorsNullary Rep.NoArguments where
  areAllConstructorsNullary _ = true

instance areAllConstructorsNullary_Argument :: AreAllConstructorsNullary (Rep.Argument a) where
  areAllConstructorsNullary _ = false

instance areAllConstructorsNullary_Product :: AreAllConstructorsNullary (Rep.Product a b) where
  areAllConstructorsNullary _ = false

data Mode = Mode
  { _Mode_ConstructorIsSingle :: Boolean
  , _Mode_ConstructorsAreAllNullary :: Boolean
  }
