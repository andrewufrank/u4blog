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

module Uniform.Markdown
  (markdownFileType
  , MarkdownText, unMT, makeMT
  , readMarkdown2
  , extMD
--   , readMarkdownFile2docrep
  )
where

  
import qualified Text.Pandoc as Pandoc
import UniformBase

import Uniform.PandocImports ( Pandoc, callPandoc )

-- readMarkdownFile2docrep  :: NoticeLevel -> Path Abs Dir -> Path Abs File -> ErrIO Docrep 
-- -- read a markdown file and convert to docrep
-- readMarkdownFile2docrep debug doughP fnin = do
--     when (inform debug) $ putIOwords 
--         ["getFile2index fnin", showPretty fnin]

--     mdfile <- read8 fnin markdownFileType 
--     pd <- readMarkdown2 mdfile
--     -- could perhaps "need" all ix as files?

--     let doc1 = pandoc2docrep doughP fnin pd
--     return doc1

     
----------------------------- -------------------------Markdown

extMD :: Extension
extMD = Extension "md"

newtype MarkdownText = MarkdownText Text
  deriving (Show, Read, Eq, Ord)

-- | a wrapper around Markdonw text
unMT :: MarkdownText -> Text
unMT (MarkdownText a) = a --needed for other ops

makeMT :: Text -> MarkdownText 
makeMT = MarkdownText 

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
            -- , Pandoc.Ext_fenced_code-block -- code blocks with ~
            , Pandoc.Ext_backtick_code_blocks
            , Pandoc.Ext_fenced_code_attributes  -- eg for haskell code snippets
            , Pandoc.Ext_auto_identifiers
            -- , Pandoc.Ext_raw_html   -- three extension give markdown_strict
            , Pandoc.Ext_raw_tex   --Allow raw TeX (other than math)
            , Pandoc.Ext_latex_macros -- allow tex for math
            , Pandoc.Ext_tex_math_dollars 
            , Pandoc.Ext_shortcut_reference_links
            , Pandoc.Ext_spaced_reference_links
            , Pandoc.Ext_footnotes  -- all footnotes
            , Pandoc.Ext_inline_notes
            , Pandoc.Ext_citations           -- <-- this is the important extension for bibTex
            , Pandoc.Ext_implicit_figures  -- a figure alone in a para will have a caption
            , Pandoc.Ext_header_attributes -- for {.unnumbered}
            , Pandoc.Ext_lists_without_preceding_blankline
            , Pandoc.Ext_superscript  -- start and closing ^
            , Pandoc.Ext_subscript -- start and closing ~
            -- , Pandoc.Ext_short_subsuperscripts  -- only start ^ and ~
            , Pandoc.Ext_strikeout  -- require ~~ two! before and after
            ]
        , Pandoc.githubMarkdownExtensions
        ]

-- instance ToJSON Text
-- writeLaTeX :: PandocMonad m => WriterOptions -> Pandoc -> m Text

-- instance TypedFiles7 Text Text where
--   wrap7 = id
--   unwrap7 = id

-- writeTexSnip2 :: Pandoc -> ErrIO Text
-- -- write a latex file from a pandoc doc
-- writeTexSnip2 pandocRes = do
--   p <- unPandocM $ writeLaTeX latexOptions pandocRes
--   return p

