--------------------------------------------------------------------------
--
-- Module      :  Uniform.PandocImports
-- | read and write pandoc files (intenal rep of pandoc written to disk)
-- von hier Pandoc spezifisches imortieren
-- nichts exportieren nach aussen
--
-- das ist, was von pandoc zum import gebraucht wird
-------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports   #-}

module Uniform.MetaStuff
  ( module Uniform.MetaStuff,
    Pandoc (..),
    Meta(..)
  )
where

import Text.Pandoc
  ( 
    Meta(..),
    -- MetaValue, nullMeta,
    -- Pandoc (..),
    -- WriterOptions
    --   ( writerCiteMethod,
    --     writerExtensions,
    --     writerHighlightStyle,
    --     writerHTMLMathMethod
    --   )
    -- , def
    -- , writeLaTeX,
  )
import qualified Text.Pandoc as Pandoc
-- import Text.Pandoc 
import Text.Pandoc.Shared (stringify, addMetaField)
-- import Text.Pandoc.Shared
import Text.Pandoc.Builder 
import Uniform.Json
import UniformBase

--  functions to extract values from the meta record 
-- containing the YAML Header values

getDoNotReplace :: Pandoc -> Maybe MetaValue 
-- get the string in the Yaml with words which must be preserved
-- a comma separated list 
getDoNotReplace pan = 
        Pandoc.lookupMeta "DoNotReplace" (getMeta pan)

getFromYaml  :: Text -> Pandoc -> Maybe MetaValue 
-- get the MetaValue in the Yaml  at the key
getFromYaml key pan = 
        Pandoc.lookupMeta key (getMeta pan)

getMetaValueFromYaml4 :: Text -> Text -> Pandoc ->  MetaValue
--get the Metavalue (with  default)
getMetaValueFromYaml4 def1 key pan = maybe (MetaString def1) (id) 
        $ getFromYaml key pan 
        
getTextFromYaml5 :: Text -> Text -> Pandoc ->  Text
--get the Metavalue (with  default)
getTextFromYaml5 def1 key pan = 
    fromJustNoteT [("getTextFromYaml5"::Text), key] .
    metaValueToText . maybe (MetaString def1) (id) 
        $ getFromYaml key pan 
        
metaValueToText :: MetaValue -> Maybe Text
metaValueToText (MetaString t) = Just t
metaValueToText (MetaInlines ils) = Just $ stringify ils
metaValueToText (MetaBlocks bls) = Just $ stringify bls
metaValueToText (MetaList xs) = unwords' <$> mapM metaValueToText xs
metaValueToText _ = Nothing

meta2pandoc :: Meta -> Pandoc 
meta2pandoc m = Pandoc m [] 

addMetaField2 :: ToMetaValue a => Text -> a -> Meta -> Meta
addMetaField2  key val meta = addMetaField key val meta

addMetaField2pandoc :: ToMetaValue a => Text -> a -> Pandoc -> Pandoc 
addMetaField2pandoc key val (Pandoc m b) = Pandoc m2 b 
    where   m2 = addMetaField2 key val m 

-- writeLaTeX2 ::    Pandoc -> ErrIO Text
-- -- gives a texsnip Text
-- writeLaTeX2 pan = unPandocM $ writeLaTeX latexOptions pan 

instance Zeros Pandoc where
  zero =  Pandoc nullMeta zero

instance Zeros Text.Pandoc.Meta where
  zero = mempty

getMeta :: Pandoc -> Pandoc.Meta
getMeta (Pandoc.Pandoc m _) = m

putMeta :: Pandoc.Meta -> Pandoc -> Pandoc
putMeta m1 (Pandoc _ p0) = Pandoc m1 p0

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

-- | Flatten a Pandoc 'Meta' into a well-structured JSON object,  
-- adapted from https://hackage.haskell.org/package/slick-1.1.1.0/docs/src/Slick.Pandoc.html#flattenMeta
-- does squish bulleted list and other markdown in YAML
-- metaValueToText seems better
flattenMeta :: Pandoc.Meta -> Value
flattenMeta (Pandoc.Meta meta) = toJSON $ fmap go meta
  where
    go :: MetaValue -> Value
    go (Pandoc.MetaMap m) = toJSON $ fmap go m
    go (Pandoc.MetaList m) = toJSONList $ fmap go m
    go (Pandoc.MetaBool m) = toJSON m
    go (Pandoc.MetaString m) = toJSON m
    go (Pandoc.MetaInlines m) = toJSON $ stringify m
    go (Pandoc.MetaBlocks m) = toJSON $ stringify m

-- readYaml2value :: Path Abs File -> ErrIO Value
-- -- | read a yaml file to a value
-- -- error when syntax issue
-- readYaml2value fp = do
--   t <- read8 fp yamlFileType
--   return . yaml2value $ t



