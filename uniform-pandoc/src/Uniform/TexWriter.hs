--------------------------------------------------------------------------
--
-- Module      :  Uniform.TexWriter
        -- to group stuff for writing Tex and later latex
-------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-unused-imports 
            #-}

module Uniform.TexWriter
  ( module Uniform.TexWriter 

--   , module Uniform.PandocImports
--     , ReaderOptions
--   , module Uniform.PandocHTMLwriter
--   , module Uniform.BibTex
--   , module Uniform.Markdown
-- --   , module Uniform.Pandoc2pdf
  )
where


import qualified Text.Pandoc                   as Pandoc
import Uniform.PandocImports
import Text.Pandoc.Highlighting (tango)
import UniformBase


latexOptions :: Pandoc.WriterOptions
-- | reasonable extension - crucial!
latexOptions =
  Pandoc.def
    { Pandoc.writerHighlightStyle = Just tango,
      Pandoc.writerCiteMethod = Pandoc.Biblatex,
      Pandoc.writerHTMLMathMethod = Pandoc.KaTeX "https://cdn.jsdelivr.net/npm/katex@0.16.8/+esm",  -- :: HTMLMathMethod 
      -- Citeproc                        -- use citeproc to render them
      --           | Natbib                        -- output natbib cite commands
      --           | Biblatex                      -- output biblatex cite commands
      Pandoc.writerExtensions =
        Pandoc.extensionsFromList
          [ Pandoc.Ext_raw_tex --Allow raw TeX (other than math)
          , Pandoc.Ext_latex_macros -- latex math 

          -- , Pandoc.Ext_shortcut_reference_links
          -- , Pandoc.Ext_spaced_reference_links
          -- , Pandoc.Ext_citations     
          , Pandoc.Ext_implicit_figures -- a figure alone will have a caption !!      
          -- <-- this is the important extension for bibTex
          ]
    }



writeTexSnip2 :: Pandoc -> ErrIO Text
-- write a latex file from a pandoc doc
writeTexSnip2 pandocRes = do
  p <- unPandocM $ Pandoc.writeLaTeX latexOptions pandocRes
  return p