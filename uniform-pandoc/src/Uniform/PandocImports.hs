--------------------------------------------------------------------------
--
-- Module      :  Uniform.PandocImports
-- | read and write pandoc files (intenal rep of pandoc written to disk)
-- von hier Pandoc spezifisches imortieren
-- nich exportieren nach aussen
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
    Pandoc (..),
  )
where

import Text.Pandoc
  ( CiteMethod (Natbib),
    Meta,
    MetaValue,
    Pandoc (..),
    WriterOptions
      ( writerCiteMethod,
        writerExtensions,
        writerHighlightStyle
      ),
    def,
    writeLaTeX,
  )
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Highlighting (tango)
import Text.Pandoc.Shared (stringify)
import Uniform.Json
      
import Uniform.Yaml  
import UniformBase
-- import Data.Aeson.Types ( parseMaybe )

  --  zero = Pandoc.Null

instance Zeros Pandoc where
  zero = Pandoc zero zero

instance Zeros Text.Pandoc.Meta where
  zero = mempty

-- | Handle possible pandoc failure within the PandocIO Monad
unPandocM :: Pandoc.PandocIO a -> ErrIO a
unPandocM op1 =
  do
    res <-
      callIO $
        Pandoc.runIO op1
    either
      ( \e -> do
          throwError . showT $ e
      )
      return
      res
    `catchError` ( \e -> do
                     throwError . showT $ e
                 )

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
    either (throwError . showT) return res
  `catchError` (throwError . showT)


getMeta :: Pandoc -> Pandoc.Meta
getMeta (Pandoc.Pandoc m _) = m

putMeta :: Pandoc.Meta -> Pandoc -> Pandoc
putMeta m1 (Pandoc _ p0) = Pandoc m1 p0

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

-- | Flatten a Pandoc 'Meta' into a well-structured JSON object,  
-- adapted from https://hackage.haskell.org/package/slick-1.1.1.0/docs/src/Slick.Pandoc.html#flattenMeta
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

readYaml2value :: Path Abs File -> ErrIO Value
-- | read a yaml file to a value
-- error when syntax issue
readYaml2value fp = do
  t <- read8 fp yamlFileType
  return . yaml2value $ t

latexOptions :: WriterOptions
-- | reasonable extension - crucial!
latexOptions =
  def
    { writerHighlightStyle = Just tango,
      writerCiteMethod = Natbib,
      -- Citeproc                        -- use citeproc to render them
      --           | Natbib                        -- output natbib cite commands
      --           | Biblatex                      -- output biblatex cite commands
      writerExtensions =
        Pandoc.extensionsFromList
          [ Pandoc.Ext_raw_tex --Allow raw TeX (other than math)
          -- , Pandoc.Ext_shortcut_reference_links
          -- , Pandoc.Ext_spaced_reference_links
          -- , Pandoc.Ext_citations           -- <-- this is the important extension for bibTex
          ]
    }

----------------------------- -------------------------Markdown
--  move to markdown TODO

extMD :: Extension
extMD = Extension "md"

newtype MarkdownText = MarkdownText Text
  deriving (Show, Read, Eq, Ord)

-- | a wrapper around Markdonw text
unMT :: MarkdownText -> Text
unMT (MarkdownText a) = a --needed for other ops

instance Zeros MarkdownText where
  zero = MarkdownText zero

markdownFileType :: TypedFile5 Text MarkdownText
markdownFileType =
  TypedFile5 {tpext5 = extMD} :: TypedFile5 Text MarkdownText

instance TypedFiles7 Text MarkdownText where
--  handling Markdown and read them into MarkdownText
  wrap7 a = MarkdownText a
  unwrap7 (MarkdownText a) = a

readMarkdown2 :: MarkdownText -> ErrIO Pandoc
-- | reads the markdown text and produces a pandoc structure
readMarkdown2 text1 =
    callPandoc $ Pandoc.readMarkdown markdownOptions (unwrap7 text1 :: Text)

-- readMarkdown3 :: Pandoc.ReaderOptions -> MarkdownText -> ErrIO Pandoc
-- readMarkdown3 options text1 =
--     unPandocM $ Pandoc.readMarkdown options (unwrap7 text1::Text)

-- | Reasonable options for reading a markdown file
markdownOptions :: Pandoc.ReaderOptions
markdownOptions = Pandoc.def { Pandoc.readerExtensions = exts }
  where
    exts = mconcat
        [ Pandoc.extensionsFromList
            [ Pandoc.Ext_yaml_metadata_block
            , Pandoc.Ext_fenced_code_attributes
            , Pandoc.Ext_auto_identifiers
            -- , Pandoc.Ext_raw_html   -- three extension give markdown_strict
            , Pandoc.Ext_raw_tex   --Allow raw TeX (other than math)
            , Pandoc.Ext_shortcut_reference_links
            , Pandoc.Ext_spaced_reference_links
            , Pandoc.Ext_footnotes  -- all footnotes
            , Pandoc.Ext_citations           -- <-- this is the important extension for bibTex
            ]
        , Pandoc.githubMarkdownExtensions
        ]

-- instance ToJSON Text
-- writeLaTeX :: PandocMonad m => WriterOptions -> Pandoc -> m Text

instance TypedFiles7 Text Text where
  wrap7 = id
  unwrap7 = id

writeTexSnip2 :: Pandoc -> ErrIO Text
-- write a latex file from a pandoc doc
writeTexSnip2 pandocRes = do
  p <- unPandocM $ writeLaTeX latexOptions pandocRes
  return p

