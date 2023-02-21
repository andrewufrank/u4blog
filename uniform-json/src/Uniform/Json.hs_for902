----------------------------------------------------------------------
--
-- Module      :  Uniform.Json
--
------------------------------------------------- --------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | the operations on JSON data types
module Uniform.Json
  ( module Uniform.Json,
    -- module Uniform.Error, -- or at least 
    ErrIO,
    Value (..),
    ToJSON (..),
    FromJSON (..),
    fromJSON,
    decode,
    omitNothingFields,
    eitherDecode,
    -- , encode
    object,
    (.=),
    genericParseJSON,
    defaultOptions,
    genericToJSON,
    fieldLabelModifier,
    HML.fromList,
    HML.toList,
    Result (..),
    encode,
    encodePretty,
    parseEither, parseMaybe
  )
where

import Control.Lens  -- aufzaehlung notwendi um := fuer aeson zu erlauben
  ( at,
    (&),
    (?~),
    (^?),
  )
import Data.Aeson
import Data.Aeson.Types 
import Data.Aeson as Aeson
import Data.Aeson.Lens (key, AsValue)
-- import Data.Aeson.Text -- was this missing ?
import Data.Aeson.Key 
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Lens -- um die underline argumente zu ermoeglichen 
import qualified Data.HashMap.Lazy as HML
import qualified Data.Aeson.KeyMap as KM -- added for ghc 9.2
import UniformBase
-- import Uniform.Error hiding (at)
-- import Uniform.Strings hiding (at)

encodeT :: ToJSON a => a -> Text
encodeT = bb2t . bl2b . encode

fromJSONmaybe :: FromJSON a => Value -> Maybe a
fromJSONmaybe v = case (fromJSON v) of
  Success a -> Just a
  _ -> Nothing

instance Zeros Value where zero = Null

fromJSONm :: (FromJSON a, Show a) => Value -> ErrIO a
-- DO NOT USE ! fromJSONm :: (FromJSON a, MonadError m) => Value -> m a
fromJSONm v = result1 (fromJSON v)
-- fromJSONm :: (FromJSON a, MonadError m) => Value -> m a

-- | the following gives error msg
fromJSONerrio :: (FromJSON a, Show a) => Value -> ErrIO a
fromJSONerrio v = callIO $ do
  result1 (fromJSON v)

fromJSONfailError :: (FromJSON a, Show a) => Value -> ErrIO a
-- | converts fromJson to a record structure
-- throws error if fails
fromJSONfailError v = case (fromJSON v) of
            Success a -> return a
            x -> throwErrorWords ["fromJson", showT x]

result1 :: (Monad m, MonadFail m) => Result a -> m a
result1 (Aeson.Error msg) = fail msg
result1 (Aeson.Success a) = return a

-- a difernt solution
-- | get a maybe value from a json value 
gak :: Data.Aeson.Lens.AsValue s => s -> Text -> Maybe Value
gak b k = (^?) b (key   $ k)

-- | get and set at a key
class AtKey vk v where
  getAtKey :: vk -> Text -> Maybe v

  getAt2Key :: vk -> Text -> Text -> Maybe v
  -- ^ two keys: one after the other

  putAtKey :: Text -> v -> vk -> vk

instance AtKey Value Text where
  getAtKey meta2 k2 = meta2 ^? key ( k2) . _String
--   getAt2Key meta2 k1 k2 = meta2 ^? key k1 . key k2 . _String
--   putAtKey k2 txt meta2 = meta2 & _Object . at k2 ?~ String txt

instance AtKey Value Integer where
--   getAtKey meta2 k2 = meta2 ^? key k2 . _Integral
--   getAt2Key meta2 k1 k2 = meta2 ^? key k1 . key k2 . _Integral
--   putAtKey k2 txt meta2 = meta2 & _Object . at k2 ?~ toJSON txt 

instance AtKey Value Int where
  getAtKey meta2 k2 = fmap fromInteger $ getAtKey meta2 k2
  getAt2Key meta2 k1 k2 = fmap fromInteger $ getAt2Key meta2 k1 k2
  putAtKey k2 v meta2 = putAtKey k2 (toInteger v) meta2  

instance AtKey Value Bool where
--   getAtKey meta2 k2 = meta2 ^? key k2 . _Bool
--   getAt2Key meta2 k1 k2 = meta2 ^? key k1 . key k2 . _Bool
--   putAtKey k2 txt meta2 = meta2 & _Object . at k2 ?~ Bool txt

instance AtKey Value Value where
  getAtKey meta2 k2 = meta2 ^? key ( k2 ) 
--   getAt2Key meta2 k1 k2 = meta2 ^? key k1 . key k2 . _Bool
--   putAtKey k2 txt meta2 = meta2 & _Object . at k2 ?~ Bool txt

class AtKey2 vk v where
  -- getAtKey :: vk -> Text -> Maybe v
  -- getAt2Key :: vk -> Text -> Text -> Maybe v

  -- ^ two keys: one after the other

  putAtKey2 :: Text -> v -> vk -> vk

instance (ToJSON a) => AtKey2 Value a where
  -- getAtKey meta2 k2 = meta2 ^? key k2 . _Integral
  -- getAt2Key meta2 k1 k2 = meta2 ^? key k1 . key k2 . _Integral
--   putAtKey2 k2 txt meta2 = meta2 & _Object . at k2 ?~ toJSON txt

mergeLeftPref ::[Value] -> Value
-- ^ The (left-biased) union of two maps.
-- all values must be objects, which can be prooduced with toJSON
-- It prefers the first map when duplicate keys are encountered,
-- http://hackage.haskell.org/package/hashmap-1.3.3/docs/Data-HashMap.html
-- for ghc 8.7  
-- mergeLeftPref = Object . HML.unions .  map unObject
-- for ghc 9.2.1
mergeLeftPref = Object .unions' .  map unObject

unions' :: [KM.KeyMap Value]  -> KM.KeyMap Value
unions' = KM.fromHashMap . HML.unions . map KM.toHashMap
-- end

mergeRightPref :: [Value] -> Value
mergeRightPref = mergeLeftPref . reverse

instance NiceStrings Value where
  shownice = bb2t . bl2b . encodePretty

unObject :: Value -> Object
unObject (Object x) = x
unObject z = errorT ["unObject in Json.hs: No Object available", "given", showT z]
