--------------------------------------------------------------------------
--
-- Module      :  Uniform.Markdown
-- these are operations which are not influenced by SSG 
-- just stuff that depends, for example, on pandoc 
-------------------------------------------------------------------------
-- {-# LANGUAGE BangPatterns                   #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports
            -fno-warn-deprecations #-}

module Uniform.Markdown
    ( module Uniform.Markdown
    , Pandoc.ReaderOptions
    , markdownFileType
    , extMD
--   , Pandoc(..)
--   , module Uniform.Error   -- or at least ErrIO
--   , write8
--   , TypedFile5
--   , TypedFiles5
--   , TypedFiles7
--   , read8
--   , module Uniform.Json
    )
where

import UniformBase 
-- import           Uniform.FileIO
import           Uniform.Json
-- import           Uniform.TypedFile -- (TypedFiles7(..))
-- import           Uniform.Yaml
-- import           Uniform.Docrep
import           Uniform.PandocImports
-- import Uniform.Pandoc as Pandoc
import Uniform.Filetypes4sites


-- import           Data.Aeson         -- for ^?, key
-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html
-- https://artyom.me/aeson
import qualified Text.Pandoc                   as Pandoc

readMarkdown2docrep :: MarkdownText -> ErrIO Docrep
-- | read a md file into a Docrep
-- all values from meta are moved to yam 
-- (meta is unused to avoid problems)
-- in a json format
readMarkdown2docrep md = do
    pd <- readMarkdown2 md
    let (Pandoc meta1 _) = pd
    let meta2                 = flattenMeta meta1
    return (Docrep meta2 pd)
        -- zero the metadata to detect errors


readMarkdown2 :: MarkdownText -> ErrIO Pandoc
readMarkdown2 text1 =
    unPandocM $ Pandoc.readMarkdown markdownOptions (unwrap7 text1)
readMarkdown3 :: Pandoc.ReaderOptions -> MarkdownText -> ErrIO Pandoc
readMarkdown3 options text1 =
    unPandocM $ Pandoc.readMarkdown options (unwrap7 text1)

-- | Reasonable options for reading a markdown file
markdownOptions :: Pandoc.ReaderOptions
markdownOptions = Pandoc.def { Pandoc.readerExtensions = exts }
  where
    exts = mconcat
        [ Pandoc.extensionsFromList
            [ Pandoc.Ext_yaml_metadata_block
            , Pandoc.Ext_fenced_code_attributes
            , Pandoc.Ext_auto_identifiers
            , Pandoc.Ext_raw_html   -- three extension give markdown_strict
            , Pandoc.Ext_raw_tex   --Allow raw TeX (other than math)
            , Pandoc.Ext_shortcut_reference_links
            , Pandoc.Ext_spaced_reference_links
            , Pandoc.Ext_footnotes  -- all footnotes
            , Pandoc.Ext_citations           -- <-- this is the important extension for bibTex
            ]
        , Pandoc.githubMarkdownExtensions
        ]

writeAST2md :: Pandoc -> ErrIO MarkdownText
-- | write the AST to markdown

writeAST2md dat = do
    r <- unPandocM $ do
        r1 <- Pandoc.writeMarkdown
            Pandoc.def { Pandoc.writerSetextHeaders = False }
            dat
        return r1
    return . wrap7 $ r

writeAST3md :: Pandoc.WriterOptions -> Pandoc -> ErrIO MarkdownText
-- | write the AST to markdown

writeAST3md options dat = do
    r <- unPandocM $ do
        r1 <- Pandoc.writeMarkdown options -- Pandoc.def { Pandoc.writerSetextHeaders = False }
                                   dat
        return r1
    return . wrap7 $ r



readMd2meta :: Path Abs File -> ErrIO (Pandoc, Value)
-- ^ read a markdown file to metadata

readMd2meta md = do
  -- putIOwords ["readMd2meta", "readPandocFile", showT md]
    mdtext :: MarkdownText <- read8 md markdownFileType
    pandoc                 <- readMarkdown2 mdtext
    let meta2 = flattenMeta (getMeta pandoc)
    -- putIOwords ["readMd2meta", "readPandocFile", showT md, "done"]
    return (pandoc, meta2)


