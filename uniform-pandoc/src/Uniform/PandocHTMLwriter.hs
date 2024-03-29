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

-- import Uniform.PandocImports (  unPandocM )
import Text.Pandoc

import Text.Pandoc.Highlighting (tango)
import qualified Text.Pandoc as Pandoc
import Uniform.PandocImports
import Text.DocLayout (render)
import Text.DocTemplates as DocTemplates

writeHtml5String2 :: Pandoc -> ErrIO Text
writeHtml5String2 pandocRes = do
    p <- unPandocM $ writeHtml5String html5Options pandocRes
    return  p

-- | Reasonable options for rendering to HTML
html5Options :: WriterOptions
html5Options =
    def
        { writerHighlightStyle = Just tango
        , writerHTMLMathMethod = Pandoc.KaTeX "https://cdn.jsdelivr.net/npm/katex@0.16.8/+esm"  -- :: HTMLMathMethod
        , writerExtensions = writerExtensions def
        }

-- | apply the template 
-- concentrating the specific pandoc ops 
applyTemplate4 ::  Bool -- ^ 
  -> Text -- ^ the template as text
  -> [Value]-- ^ the values to fill in (produce with toJSON)
  -- possibly Map (Text, Text) from Data.Map 
  -> ErrIO Text -- ^ the resulting html text 
applyTemplate4 debug t1 vals = do
    templ1 <- liftIO $ DocTemplates.compileTemplate mempty t1
    -- err1 :: Either String (Doc Text) <- liftIO $ DocTemplates.applyTemplate mempty (unwrap7 templText) (unDocValue val)
    
    let templ3 = case templ1 of
            Left msg -> errorT ["applyTemplate4 error", showT msg]
            Right tmp2 -> tmp2
    when debug $ putIOwords ["applyTemplate3 temp2",  showT templ3]
    -- renderTemplate :: (TemplateTarget a, ToContext a b) => Template a -> b -> Doc a
    let valmerged = mergeLeftPref vals
    when debug $ putIOwords ["the valmerged is ", showPretty valmerged]
    let res = renderTemplate templ3 ( valmerged)
    -- when debug $ putIOwords ["applyTemplate3 res",  showT res]
    let res2 = render Nothing res  -- macht reflow (zeileneinteilung)
    return res2

writeAST2md :: Pandoc -> ErrIO Text

-- | write the AST to markdown
writeAST2md dat = do
    r <- unPandocM $ do
        r1 <-
            Pandoc.writeMarkdown
                Pandoc.def{Pandoc.writerSetextHeaders = False}
                dat
        return r1
    return  r

writeAST3md :: Pandoc.WriterOptions -> Pandoc -> ErrIO Text

-- | write the AST to markdown
writeAST3md options dat = do
    r <- unPandocM $ do
        r1 <-
            Pandoc.writeMarkdown
                options -- Pandoc.def { Pandoc.writerSetextHeaders = False }
                dat
        return r1
    return r
