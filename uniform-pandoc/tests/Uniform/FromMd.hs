---------------------------------------------------------------------------
--
-- Module      :  pandoc test the construction of the html and the latex files
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.FromMd where

import Test.Framework
-- import qualified Data.Map as M 
---- using uniform:
import Uniform.Json hiding (fromList)
import Uniform.PandocImports 
import Uniform.Markdown 
import Uniform.TexWriter
import Uniform.PandocHTMLwriter
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Writers
import Text.Pandoc.Writers.Shared 
import Text.DocTemplates as DocTemplates
import Text.DocLayout (render)
import Data.Text.Lazy (unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Map 
-- import Uniform.Filenames 
-- import Uniform2.Filetypes4sites should not be imported here

import Uniform.Test.TestHarness
import Uniform.MetaStuff_test 
-- import Uniform.Markdown_test 
import Uniform.MetaStuff
import Uniform.TemplatesStuff
-- import Uniform.Error           hiding (  (<.>)  )  -- (</>)
import UniformBase
import Uniform.HttpFiles
import Uniform.TexFileTypes

defFieldText :: Text -> Text -> Context Text -> Context Text 
defFieldText k v ct = defField k v  ct

fillContext1 :: f   -> Context Text -> Context Text 
fillContext1 fn ct  = defFieldText "linkpdf3" "A.pdf" -- convertLink2pdf ix 
                    . defFieldText "link" "filename3" -- convertLink2html ix=fn 
                    -- here all hmtl + latex  context values 
                    $ ct 

-- ? how to deal with defaults?
fillContextHtml :: ToContext Text a => a -> Context Text -> Context Text 
fillContextHtml content ct = defField "body" content  
                     $ ct
fillContextLatex :: ToContext Text a => a -> Context Text -> Context Text 
fillContextLatex content ct = defFieldText "documentclass" "article"
                    . defFieldText "fontsize" "12pt"
                    . defField "content3" content  
                    $ ct

fnA = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/startValues/A.md"
fnres_html =  makeAbsFile "/home/frank/tests/testhtmlA"

test_A = do 
    res1 <- runErr $ do 
        mdfile <- read8 fnA markdownFileType 
        pd@(Pandoc m1 p1) <- readMarkdown2 mdfile

        putIOwords ["pd \n", showT pd, "\n--"]

        let contextBasic = fillContext1 fnA (mempty:: Context Text) :: Context Text 

        contentHtml <- writeHtml5String2 pd   -- adds the content 
        contentTex <- writeTexSnip2 pd

        let ctHtml = fillContextHtml contentHtml contextBasic
            ctLatex = fillContextLatex contentTex contextBasic

        putIOwords ["ctHtml", showT ctHtml]

        templH <- compileDefaultTempalteHTML 
            -- templT = compileDefaultTempalteLatex
        let restplH = renderTemplate templH ctHtml :: Doc Text
        let resH = render (Just 50) restplH  :: Text  -- line length, can be Nothing
        putIOwords ["resH", resH]
        let resHtml = HTMLout resH
        write8   fnres_html htmloutFileType resHtml

        return "A"
    -- let Right (target3, res3) = res5
    assertEqual (Right "") res1

-- Pandoc (Meta {unMeta = fromList [("abstract",MetaInlines [Str "An",Space,Emph [Str "abstract"],Space,Str "for",Space,Str "the",Space,Strong [Str "example"],Space,Str "A"]),("date",MetaInlines [Str "2020-06-16"]),("keywords",MetaInlines [Str "A_KEYword"]),("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title",Space,Str "of",Space,Str "A"])]}) [Header 1 ("hl1_text-for-title-a",[],[]) [Str "hl1_text",Space,Emph [Str "For"],Space,Strong [Str "Title"],Space,Str "A"],Para [Str "Nonsense",Space,Str "list."],BulletList [[Plain [Str "one"]],[Plain [Str "two"]]]] 