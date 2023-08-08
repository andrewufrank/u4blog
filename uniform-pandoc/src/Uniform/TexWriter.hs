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
import Uniform.TexFileTypes
import qualified Data.Map as M

import Data.Map (fromList, toList)
import Uniform.TemplateStuff
import Text.DocTemplates as DocTemplates ( Doc )

import Uniform.MetaStuff

meta2latex ::  Template Text -> Meta -> ErrIO Latex
-- step2: the second part resulting in HTML result
-- requires compiled template 
-- and sets documentclass to article
meta2latex  templL  meta = do
    putIOwords ["meta2hres meta \n", showT meta, "\n--"]
    -- convert to list of (text,Block) 
    -- make M.Map and pass to render template 

    -- add docclass 
    let meta2 = addMetaFieldT "documentclass" "article" meta
    t  :: M.Map Text Text <- meta2xx   writeTexSnip2 meta2
    putIOwords ["meta2hres tHtml \n", showT t, "\n--"]

    -- templL :: Template Text <- compileDefaultTempalteLatex
        -- templL :: Template Text  <-compileDefaultTempalteLatex
        -- -- renderTemplate :: (TemplateTarget a, ToContext a b) => Template a -> b -> Doc a
    let restpl = renderTemplate templL t :: Doc Text
    let resH = render (Just 50) restpl :: Text  -- line length, can be Nothing
        -- let restplL = renderTemplate templL ctLatex :: Doc Text
        -- let resL = render (Just 50) restplL  :: Text  -- line length, can be Nothing    -- todo 
    return (Latex resH)

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