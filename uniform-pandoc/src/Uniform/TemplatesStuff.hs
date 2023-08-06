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

module Uniform.TemplatesStuff
  ( module Uniform.TemplatesStuff,
    Pandoc (..),
    Template, renderTemplate, render
  )
where


-- import Uniform.Json ( Value )
import UniformBase

import Uniform.PandocImports (  unPandocM )
import Text.Pandoc
    ( renderTemplate,
      compileDefaultTemplate,
      getDefaultTemplate,
      Template,
      Pandoc(..) )






-- import Text.Pandoc.Highlighting  
import qualified Text.Pandoc as Pandoc
-- import Uniform.PandocImports
import Text.DocLayout (render)
import qualified Text.DocTemplates as DocTemplates
-- import Data.Either (fromLeft, fromRight)

instance Zeros (Template Text) where 
        zero = mempty -- Empty :: Template Text

getDefaultTemplateLatex :: ErrIO Text 
getDefaultTemplateLatex =   unPandocM $ getDefaultTemplate "latex"
getDefaultTemplateHTML :: ErrIO Text 
getDefaultTemplateHTML = unPandocM $ getDefaultTemplate "html"

compileDefaultTempalteHTML :: ErrIO (Template Text )
compileDefaultTempalteHTML = unPandocM $ compileDefaultTemplate "html"
compileDefaultTempalteLatex :: ErrIO (Template Text) 
compileDefaultTempalteLatex = unPandocM $ compileDefaultTemplate "latex"

compileTemplateText ::  
    Text -- ^ the template as text
    -> ErrIO (Template Text)
compileTemplateText tplText = do     
    templ1 <- liftIO $ DocTemplates.compileTemplate mempty tplText
    -- err1 :: Either String (Doc Text) <- liftIO $ DocTemplates.applyTemplate mempty (unwrap7 templText) (unDocValue val)
    let templ3 = case templ1 of
            Left msg -> errorT ["compileTemplateFile error", showT msg]
            Right tmp2 -> tmp2
    -- putIOwords ["compileTemplateFile templ3",  showT templ3]
    return templ3

compileTemplateFile2 tplfn = do 
    tp <- readFile2 tplfn 
    ctpl <- compileTemplateText tp 
    return ctpl 



-- | apply the template 
-- concentrating the specific pandoc ops 
-- not used; use compileTemplateText and meta2hres/latex
-- applyTemplate4 ::  Bool -- ^ 
--   -> Text -- ^ the template as text
--   -> [Value]-- ^ the values to fill in (produce with toJSON)a
--   -- possibly Map (Text, Text) from Data.Map 
--   -> ErrIO Text -- ^ the resulting html text 
-- applyTemplate4 debug t1 vals = do
--     templ3 <- compileTemplateText debug t1
--     -- when debug $ 
--     putIOwords ["applyTemplate3 temp2",  showT templ3]
--     -- renderTemplate :: (TemplateTarget a, ToContext a b) => Template a -> b -> Doc a
--     let valmerged = vals -- mergeLeftPref vals
--     when debug $ putIOwords ["the valmerged is ", showPretty valmerged]
--     let res = renderTemplate templ3 ( valmerged)
--     -- when debug $ putIOwords ["applyTemplate3 res",  showT res]
--     let res2 = render Nothing res  -- macht reflow (zeileneinteilung)
--     return res2

-- writeAST2md :: Pandoc -> ErrIO Text

-- -- | write the AST to markdown (def + set extHeaders False)
-- writeAST2md dat = do
--     r <- unPandocM $ do
--         r1 <-
--             Pandoc.writeMarkdown
--                 Pandoc.def{Pandoc.writerSetextHeaders = False}
--                 dat
--         return r1
--     return  r

-- writeAST3md :: Pandoc.WriterOptions -> Pandoc -> ErrIO Text

-- | write the AST to markdown with options
writeAST3md options dat = do
    r <- unPandocM $ do
        r1 <-
            Pandoc.writeMarkdown
                options  
                dat
        return r1
    return r

-- | write the AST to html with options
writeAST3html options dat = do
    r <- unPandocM $ do
        r1 <-
            Pandoc.writeHtml5
                options  
                dat
        return r1
    return r
