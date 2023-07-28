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

type Con = Context MetaValue
defFieldText :: Text -> Text -> Con -> Con 
-- set the value if it has not value so far, does not overwrite
defFieldText k v ct = defField k (MetaString v)  ct

-- defField :: ToContext a b => Text -> b -> Context a -> Context a

fillContext1 :: fn  -> Con -> Con 
fillContext1 fn ct  = defFieldText "linkpdf3" "A.pdf" -- convertLink2pdf ix 
                    . defFieldText "link" "filename3" -- convertLink2html ix=fn 
                    -- here all hmtl + latex  context values 
                    $ ct 
fillMeta2context :: Pandoc -> Con -> Con 
-- the values in meta from the YAML header which are text 
fillMeta2context pan ct =
                    moveFieldMeta2Context zero  "date" pan

                    $ ct 

-- ? how to deal with defaults?
fillContextHtml ::   Text -> Con -> Con 
fillContextHtml content ct = defField "body" (MetaString content)  
                     $ ct
fillContextLatex :: Text -> Con -> Con 
fillContextLatex content ct = defFieldText "documentclass" "article"
                    . defFieldText "fontsize" "12pt"
                    . defField "body" (MetaString content)  
                    $ ct

moveFieldMeta2Context ::  Text ->  Text -> Pandoc ->Con -> Con 
-- get a field from the metadata and put it in the context  as text 
moveFieldMeta2Context def k pan  ct = defFieldText k v ct 
    where   v =   getTextFromYaml5 def k pan  :: Text                 

moveFieldMetaVal2Context ::  Text ->  Text -> Pandoc ->Con -> Con 
-- get a field from the metadata and put it in the context as metavalue 
moveFieldMetaVal2Context def k pan  ct = defField k v ct 
    where   v =   getMetaValueFromYaml4 def k pan  :: MetaValue                

fnA = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/startValues/A.md"
fnres_html =  makeAbsFile "/home/frank/tests/testhtmlA"
fnres_latex =  makeAbsFile "/home/frank/tests/testlatexA"

test_A = do 
    res1 <- runErr $ do 
        mdfile <- read8 fnA markdownFileType 
        pd@(Pandoc m1 p1) <- readMarkdown2 mdfile

        putIOwords ["pd \n", showT pd, "\n--"]

        -- get the metadata 
        let contextFromMeta = fillMeta2context pd (mempty:: Con)
        let contextFromMetaVal =     -- the ones to preserve the format
                moveFieldMetaVal2Context "abstract missing" "abstract" pd
                . moveFieldMetaVal2Context "title missing" "title" pd
                . moveFieldMetaVal2Context "author missing" "author" pd
                $ contextFromMeta :: Con
        let contextBasic = fillContext1 fnA  contextFromMetaVal:: Con 

        contentHtml :: Text <- writeHtml5String2 pd   -- adds the content 
        contentTex :: Text <- writeTexSnip2 pd

        let ctHtml = fillContextHtml contentHtml contextBasic :: Con
            ctLatex = fillContextLatex contentTex contextBasic :: Con 

        putIOwords ["ctHtml", showT ctHtml]

        templH :: Template Text <- compileDefaultTempalteHTML 
        templL :: Template Text  <-compileDefaultTempalteLatex
        -- renderTemplate :: (TemplateTarget a, ToContext a b) => Template a -> b -> Doc a
        let restplH = renderTemplate templH ctHtml :: Doc Text
        let resH = render (Just 50) restplH  :: Text  -- line length, can be Nothing
        let restplL = renderTemplate templL ctLatex :: Doc Text
        let resL = render (Just 50) restplL  :: Text  -- line length, can be Nothing
        
        putIOwords ["resH", resH]

        write8   fnres_html htmloutFileType (HTMLout resH)
        write8   fnres_latex texFileType (Latex resL)

        return "A"
    -- let Right (target3, res3) = res5
    assertEqual (Right "") res1

-- Pandoc (Meta {unMeta = fromList [("abstract",MetaInlines [Str "An",Space,Emph [Str "abstract"],Space,Str "for",Space,Str "the",Space,Strong [Str "example"],Space,Str "A"]),("date",MetaInlines [Str "2020-06-16"]),("keywords",MetaInlines [Str "A_KEYword"]),("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title",Space,Str "of",Space,Str "A"])]}) [Header 1 ("hl1_text-for-title-a",[],[]) [Str "hl1_text",Space,Emph [Str "For"],Space,Strong [Str "Title"],Space,Str "A"],Para [Str "Nonsense",Space,Str "list."],BulletList [[Plain [Str "one"]],[Plain [Str "two"]]]] 