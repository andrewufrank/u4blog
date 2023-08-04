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
-- {-# LANGUAGE DeriveGeneric #-}
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

-- import Text.Pandoc
--   (
--     Meta(..),
--     -- MetaValue, nullMeta,
--     -- Pandoc (..),
--     -- WriterOptions
--     --   ( writerCiteMethod,
--     --     writerExtensions,
--     --     writerHighlightStyle,
--     --     writerHTMLMathMethod
--     --   )
--     -- , def
--     -- , writeLaTeX,
--   )
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Shared (stringify, addMetaField)
import Text.Pandoc.Builder (ToMetaValue) -- do not import more!
import Uniform.Json hiding ( fromList, toList) 
import UniformBase
import Uniform.PandocImports  
import qualified Data.Map as M 
import Data.Map ( fromList, toList) 
import Text.Pandoc
import Uniform.Markdown
import qualified Text.Pandoc.Citeproc as PC


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
getMetaValueFromYaml4 def1 key pan = fromMaybe (MetaString def1)
        $ getFromYaml key pan

getTextFromYaml5 :: Text -> Text -> Pandoc ->  Text
--get the Metavalue (with  default)
getTextFromYaml5 def1 key pan =
    fromJustNoteT ["getTextFromYaml5"::Text, key] .
    metaValueToText . fromMaybe (MetaString def1)
        $ getFromYaml key pan

metaValueToText :: MetaValue -> Maybe Text
metaValueToText (MetaString t) = Just t
metaValueToText (MetaInlines ils) = Just $ stringify ils
metaValueToText (MetaBlocks bls) = Just $ stringify bls
metaValueToText (MetaList xs) = unwords' <$> mapM metaValueToText xs
metaValueToText _ = Nothing

metaValueToBlock :: MetaValue -> Maybe [Block]
metaValueToBlock (MetaString t) = Just . sing $ Plain [Str t]
metaValueToBlock (MetaBool t) = Just . sing $ Plain [Str . showT $ t]
metaValueToBlock (MetaInlines ils) = Just . sing$ Plain ils  -- could be Para
metaValueToBlock (MetaBlocks bls) = Just $ bls
-- metaValueToBlock (MetaList xs) = unwords' <$> mapM metaValueToText xs
metaValueToBlock _ = Nothing

sing a = [a]


-- block2htmltext  :: [Block] -> ErrIO Text
-- -- convert a Block to a html text 
-- block2htmltext b = writeHtml5String2 (Pandoc nullMeta b)

-- metaValueToHTML :: MetaValue -> ErrIO Text
-- metaValueToHTML mv = do 
--     let bs = metaValueToBlock mv ::Maybe [Block]
--     t <- block2htmltext (fromJustNote "metaValueToHTML" $ bs)
--     return t

block2xx  :: (Pandoc -> ErrIO Text) ->[Block] -> ErrIO Text
-- convert a Block to a html text 
block2xx writeXX b = writeXX (Pandoc nullMeta b)

metaValue2xx :: (Pandoc -> ErrIO Text) -> MetaValue -> ErrIO Text
metaValue2xx writer mv = do 
    let bs = metaValueToBlock mv ::Maybe [Block]
    t <- block2xx writer (fromJustNote ("metaValueTo2xx " <> show mv) $ bs)
    return t

meta2xx :: (Pandoc -> ErrIO Text) -> Meta -> ErrIO (M.Map Text Text)
-- convert all in Meta to html codes
meta2xx writer  m1 = do
    let listMetaValues = toList . unMeta $ m1:: [(Text, MetaValue)]
    l2 <- mapM mapSec listMetaValues
    return $ fromList l2
  where 
    mapSec :: (Text, MetaValue) -> ErrIO (Text, Text)
    mapSec (t, mv) = do  
        mv2 :: Text <- metaValue2xx writer mv
        return (t, mv2) -- fromJustNote "meta2htmltext" $ mv2)

md2Meta :: Path Abs File -> MarkdownText -> ErrIO Meta
-- step1: convert a markdown file to MetaValue
-- independent of output target (html or latex)
md2Meta  filep mdtext = do
    putIOwords ["md2Meta filepath", showT  filep, "\n--"] 
    pandoc1<- readMarkdown2 mdtext
    -- putIOwords ["pd \n", showT pd, "\n--"] 
    -- process references, does nothing if none
    (Pandoc m1 p1) <- unPandocM $ PC.processCitations pandoc1
    let c1 = Meta (fromList [(("body"::Text), (MetaBlocks p1))])
    -- todo missing the index, references, umlaut conversion
    let    cn = mergeAll [m1, c1] :: Meta-- order may  be important
    putIOwords ["md2Meta cn \n", showT cn, "\n--"]
    return cn

mergeAll :: [Meta] -> Meta
mergeAll  = Meta . fromList . concat . map toList . map unMeta
meta2pandoc :: Meta -> Pandoc
meta2pandoc m = Pandoc m []

addMetaFieldT ::   Text -> Text -> Meta -> Meta
addMetaFieldT = addMetaField

addMetaField2pandoc :: ToMetaValue a => Text -> a -> Pandoc -> Pandoc
addMetaField2pandoc key val (Pandoc m b) = Pandoc m2 b
    where   m2 = addMetaField key val m

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



