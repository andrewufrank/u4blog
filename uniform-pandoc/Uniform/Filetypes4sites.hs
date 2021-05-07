---------------------------------------------------------------------------
--
-- Module      : all the filetype used for SSG 

-- | SSG uses sequences of transformations between data structures (types)
-- MD -> Docrep -> Panrep -> TexSnip -> Tex -> PDF 
--                 Pandrep ->HTML 
-- Each result is written as a typed file with a specific extension
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Uniform.Filetypes4sites where   

import Uniform.PandocImports (Pandoc, Panrep (Panrep), unPandocM)
import Uniform.Json

import UniformBase

-------------------- fileType ----------
-- extCSL = Extension "csl"
-- cslFileType = TypedFile5 {tpext5 = extCSL} :: TypedFile5 Text Style

-- instance TypedFiles7 Text Style where 
--     wrap7 = id 
--     unwrap7 = id 

---------------------------------
-- extBib = Extension "bib"
-- bibFileType = TypedFile5 {tpext5 = extBib}

-- instance TypedFiles7 Text 
----------------------------- Markdown 
extMD = Extension "md"

newtype MarkdownText = MarkdownText Text
  deriving (Show, Read, Eq, Ord)

-- a wrapper around Markdonw text
unMT (MarkdownText a) = a   --needed for other ops

instance Zeros MarkdownText where
    zero = MarkdownText zero

markdownFileType =
    TypedFile5 { tpext5 = extMD } :: TypedFile5 Text MarkdownText


instance TypedFiles7 Text MarkdownText where
  -- handling Markdown and read them into MarkdownText
    wrap7 = MarkdownText

    unwrap7 (MarkdownText a) = a
--------------------------------------------typed file DocRep
-- | representation of a document
-- the yam part contains the json formated yaml metadata
-- which is extensible
-- Attention the Pandoc is Pandoc (Meta (Map Text MetaValue) [Block]
-- means that title etc is duplicated in the Meta part.
-- it would be better to use only the block and all
-- metadata keept in the yam json
-- TODO replace Pandoc with Block in DocRep
data DocRep = DocRep {yam :: Value, pan :: Pandoc} -- a json value
  deriving (Show, Read, Eq, Generic)

instance Zeros DocRep where zero = DocRep zero zero

instance FromJSON DocRep

instance ToJSON DocRep

extDocRep :: Extension
extDocRep = Extension "docrep"

-- instance NiceStrings DocRep where
--   shownice = showNice . unDocRep

docRepFileType :: TypedFile5 Text DocRep
docRepFileType =
  TypedFile5 {tpext5 = extDocRep} :: TypedFile5 Text DocRep

instance TypedFiles7 Text DocRep where
  wrap7 = readNote "DocRep wrap7 sfasdwe" . t2s
  unwrap7 = showT