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

module Uniform.PandocImports
  ( module Uniform.PandocImports,
    Pandoc (..)
  )
where

import Text.Pandoc
  ( 
    Meta,
    MetaValue, nullMeta,
    Pandoc (..),
    WriterOptions
      ( writerCiteMethod,
        writerExtensions,
        writerHighlightStyle,
        writerHTMLMathMethod
      )
    , def
    , writeLaTeX,
  )
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Shared (stringify)
import Uniform.Json
import UniformBase

--  functions to extract values from the meta record 
-- containing the YAML Header values

getDoNotReplace :: Pandoc -> Text 
-- get the string in the Yaml with words which must be preserved
-- a comma separated list 
getDoNotReplace pan = zero 


instance Zeros Pandoc where
  zero =  Pandoc nullMeta zero

instance Zeros Text.Pandoc.Meta where
  zero = mempty

-- | Handle possible pandoc failure within the PandocIO Monad
unPandocM :: Pandoc.PandocIO a -> ErrIO a
unPandocM op1 = callPandoc op1
--   do
--     res <-
--       callIO $
--         Pandoc.runIO op1
--     either
--       ( \e -> do
--           throwErrorT [e]
--       )
--       return
--       res
--     `catchError` ( \e -> do
--                      throwErrorT [e]
--                  )

-- callPandoc :: Pandoc.PandocIO a -> ErrIO a
-- callPandoc op1 = 
--     callIO $ Pandoc.runIO op1
--     >>= 
--     either (\e -> throwError . showT $  e)  return  
--   `catchError` (\e -> throwError . showT $ e)

-- callPandoc1 :: Pandoc.PandocIO a -> ErrIO a
-- callPandoc1 op1 = 
--     callIO $ Pandoc.runIO op1
--     >>= 
--     either ( throwError . showT  )  return  
--  `catchError` (throwError . showT)

callPandoc :: Pandoc.PandocIO a -> ErrIO a
callPandoc op1 = do
    res <- callIO $ Pandoc.runIO op1
    either (throwErrorT . showT) return res
  `catchError` (throwErrorT . showT)


getMeta :: Pandoc -> Pandoc.Meta
getMeta (Pandoc.Pandoc m _) = m

putMeta :: Pandoc.Meta -> Pandoc -> Pandoc
putMeta m1 (Pandoc _ p0) = Pandoc m1 p0

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

-- | Flatten a Pandoc 'Meta' into a well-structured JSON object,  
-- adapted from https://hackage.haskell.org/package/slick-1.1.1.0/docs/src/Slick.Pandoc.html#flattenMeta
-- does squish bulleted list and other markdown in YAML
flattenMeta :: Pandoc.Meta -> Value
flattenMeta (Pandoc.Meta meta) = toJSON $ fmap go meta
  where
    go :: MetaValue -> Value
    go (Pandoc.MetaMap m) = toJSON $ fmap go m
    go (Pandoc.MetaList m) = toJSONList $ fmap go m
    go (Pandoc.MetaBool m) = toJSON m
    go (Pandoc.MetaString m) = toJSON m
    -- go (Pandoc.MetaInlines m) = toJSON $ stringify m
    -- go (Pandoc.MetaBlocks m) = toJSON $ stringify m

-- readYaml2value :: Path Abs File -> ErrIO Value
-- -- | read a yaml file to a value
-- -- error when syntax issue
-- readYaml2value fp = do
--   t <- read8 fp yamlFileType
--   return . yaml2value $ t



