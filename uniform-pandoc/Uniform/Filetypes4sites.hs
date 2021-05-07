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

import Uniform.PandocImports 
-- (Pandoc, Panrep (Panrep), unPandocM)
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

-------------------- fileType Panrep ----------
-- a file containing what pandoc internally works on 
extPanrep = Extension "panrep"

panrepFileType =
  TypedFile5 {tpext5 = extPanrep} :: TypedFile5 Text Panrep

data Panrep = Panrep {panyam :: Value, panpan :: Pandoc}
  deriving (Eq, Show, Read)

instance Zeros Panrep where zero = Panrep zero zero

instance TypedFiles7 Text Panrep where
  -- handling Pandoc and read them into PandocText
  wrap7 = readNote "wrap7 for pandoc 223d" . t2s
  unwrap7 = showT

--------------------  TexSnip 
-- a tex snip is a piece of latex code, but not a full compilable
-- latex which results in a pdf

extTexSnip :: UniformBase.Extension
extTexSnip = Extension "texsnip"

-- | a wrapper around TexSnip
data TexSnip = TexSnip {snipyam :: Value, unTexSnip :: Text}
  deriving (Show, Read, Eq)

-- unTexSnip (TexSnip a) = a   --needed for other ops

instance Zeros TexSnip where
  zero = TexSnip zero zero

texSnipFileType :: TypedFile5 Text TexSnip
texSnipFileType =
  TypedFile5 {tpext5 = extTexSnip} :: TypedFile5 Text TexSnip

instance TypedFiles7 Text TexSnip where
  -- handling TexSnip and read them into TexSnipText
  -- the file on disk is readable for texstudio

  wrap7 = readNote "wrap7 for TexSnip dwe11d" . t2s
  unwrap7 = showT

----------------  Tex 

extTex = Extension "tex"

texFileType = TypedFile5 {tpext5 = extTex} :: TypedFile5 Text Latex

instance TypedFiles7 Text Latex where
  wrap7 = Latex
  unwrap7 = unLatex

newtype Latex = Latex {unLatex :: Text}
  deriving (Eq, Ord, Read, Show)

-- this is a full file, not just a snippet

instance Zeros Latex where
  zero = Latex zero

---------------------------------------------- PDF

extPDF = Extension "pdf"

pdfFileType = TypedFile5 {tpext5 = extPDF} :: TypedFile5 Text PDFfile

newtype PDFfile = PDFfile {unpdffile :: Text}
  deriving (Eq, Ord, Read, Show)

instance Zeros PDFfile where
  zero = PDFfile zero

instance TypedFiles7 Text PDFfile where
  wrap7 = PDFfile
  unwrap7 = unpdffile
