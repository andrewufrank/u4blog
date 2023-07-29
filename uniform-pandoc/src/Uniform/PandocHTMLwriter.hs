--------------------------------------------------------------------------
--
-- Module      :  Uniform.PandocImports
-- | read and write pandoc files (intenal rep of pandoc written to disk)
-- von hier Pandoc spezifisches imortieren
-- nich exportieren nach aussen
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

module Uniform.PandocHTMLwriter
  ( module Uniform.PandocHTMLwriter,
    Pandoc (..),
  )
where


import Uniform.Json
import UniformBase

import Uniform.PandocImports (  unPandocM )
import Text.Pandoc

import Text.Pandoc.Highlighting  
import qualified Text.Pandoc as Pandoc
-- import Uniform.PandocImports
-- import Text.DocLayout (render)
-- import Text.DocTemplates as DocTemplates

writeHtml5String2 :: Pandoc -> ErrIO Text
writeHtml5String2 pandocRes = do
    p <- unPandocM $ writeHtml5String html5Options pandocRes
    return  p


-- | Reasonable options for rendering to HTML
html5Options :: WriterOptions
html5Options =
    def
        { writerHighlightStyle = Just tango
        , writerHTMLMathMethod = Pandoc.MathML -- :: HTMLMathMethod
        -- , writerExtensions = -- writerExtensions def
                --  WriterOptions { writerTemplate         = Nothing
                --       , writerVariables        = mempty
                --       , writerTabStop          = 4
                --       , writerTableOfContents  = False
                --       , writerIncremental      = False
                --       , writerHTMLMathMethod   = PlainMath
                --       , writerNumberSections   = False
                --       , writerNumberOffset     = [0,0,0,0,0,0]
                --       , writerSectionDivs      = False
                --       , writerExtensions       = emptyExtensions
                --       , writerReferenceLinks   = False
                --       , writerDpi              = 96
                --       , writerWrapText         = WrapAuto
                --       , writerColumns          = 72
                --       , writerEmailObfuscation = NoObfuscation
                --       , writerIdentifierPrefix = ""
                --       , writerCiteMethod       = Citeproc
                      , writerHtmlQTags        = True -- changed af
                    --   , writerSlideLevel       = Nothing
                    --   , writerTopLevelDivision = TopLevelDefault
                    --   , writerListings         = False
                    --   , writerHighlightStyle   = Just pygments
                    --   , writerSetextHeaders    = False
                    --   , writerListTables       = False
                    --   , writerEpubSubdirectory = "EPUB"
                    --   , writerEpubMetadata     = Nothing
                    --   , writerEpubFonts        = []
                    --   , writerEpubTitlePage    = True
                    --   , writerSplitLevel       = 1
                    --   , writerChunkTemplate    = "%s-%i.html"
                    --   , writerTOCDepth         = 3
                    --   , writerReferenceDoc     = Nothing
                    --   , writerReferenceLocation = EndOfDocument
                    -- --   , writerSyntaxMap        = defaultSyntaxMap
                    --   , writerPreferAscii      = False
                    --   }

        }
