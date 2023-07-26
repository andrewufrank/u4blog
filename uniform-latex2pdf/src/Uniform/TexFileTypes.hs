----------------------------------------------------------------
--
-- Module      :  Uniform.latex
--
-- | convert latex to pdf 
---------------------------------------------------------------
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}

{-# OPTIONS_GHC -w #-}

module Uniform.TexFileTypes
  ( module Uniform.TexFileTypes
--   , writePDF2
  ) where

import Uniform.PandocImports
import Text.DocTemplates as DocTemplates
import Text.DocLayout (render)

import UniformBase
import Data.Aeson

--------------------  TexSnip

-- extTexSnip :: UniformBase.Extension
-- extTexSnip = Extension "texsnip"

-- {- | a wrapper around TexSnip
--  snipyam is not used
-- a tex snip is a piece of latex code, but not a full compilable
-- latex which results in a pdf
-- -}
-- data TexSnip = TexSnip {snipyam :: MetaPage, unTexSnip :: Text}
--     deriving (Show, Read, Eq)

-- -- unTexSnip (TexSnip a) = a   --needed for other ops

-- instance Zeros TexSnip where
--     zero = TexSnip zero zero

-- texSnipFileType :: TypedFile5 Text TexSnip
-- texSnipFileType =
--     TypedFile5{tpext5 = extTexSnip} :: TypedFile5 Text TexSnip

-- instance TypedFiles7 Text TexSnip where
--     -- handling TexSnip and read them into TexSnipText
--     -- the file on disk is readable for texstudio

--     wrap7 = readNote "wrap7 for TexSnip dwe11d" . t2s
--     unwrap7 = showT

----------------  Tex

extTex :: Extension
extTex = Extension "tex"

texFileType :: TypedFile5 Text Latex
texFileType = TypedFile5{tpext5 = extTex} :: TypedFile5 Text Latex

instance TypedFiles7 Text Latex where
    wrap7 = Latex
    unwrap7 = unLatex

-- | this is a full file, not just a snippet
newtype Latex = Latex {unLatex :: Text}
    deriving (Eq, Ord, Read, Show)

instance Zeros Latex where
    zero = Latex zero

---------------------------------------------- PDF
-- extension in metapage

extPDF :: Extension
extPDF = Extension "pdf"

pdfFileType :: TypedFile5 Text PDFfile
pdfFileType = TypedFile5{tpext5 = extPDF} :: TypedFile5 Text PDFfile

-- | a file in PDF format
newtype PDFfile = PDFfile {unpdffile :: Text}
    deriving (Eq, Ord, Read, Show)

instance Zeros PDFfile where
    zero = PDFfile zero

instance TypedFiles7 Text PDFfile where
    wrap7 = PDFfile
    unwrap7 = unpdffile