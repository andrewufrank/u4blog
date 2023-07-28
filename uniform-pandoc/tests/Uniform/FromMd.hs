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
import Uniform.Json hiding (toList, fromList)
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


-- defFieldText :: Text -> Text -> Meta -> Meta 
-- -- set the value if it has not value so far, does not overwrite
-- defFieldText k v ct = defField k (MetaString v)  ct

-- -- defField :: ToContext a b => Text -> b -> Context a -> Context a

-- fillContext1 :: fn  -> Meta -> Meta 
-- fillContext1 fn ct  = defFieldText "linkpdf3" "A.pdf" -- convertLink2pdf ix 
--                     . defFieldText "link" "filename3" -- convertLink2html ix=fn 
--                     -- here all hmtl + latex  context values 
--                     $ ct 
-- fillMeta2context :: Pandoc -> Meta -> Meta 
-- -- the values in meta from the YAML header which are text 
-- fillMeta2context pan ct =
--                     moveFieldMeta2Context zero  "date" pan

--                     $ ct 

-- -- ? how to deal with defaults?
-- fillContextHtml ::   Text -> Meta -> Meta 
-- fillContextHtml content ct = defField "body" (MetaString content)  
--                      $ ct
-- fillContextLatex :: Text -> Meta -> Meta 
-- fillContextLatex content ct = defFieldText "documentclass" "article"
--                     . defFieldText "fontsize" "12pt"
--                     . defField "body" (MetaString content)  
--                     $ ct

-- moveFieldMeta2Context ::  Text ->  Text -> Pandoc ->Meta -> Meta 
-- -- get a field from the metadata and put it in the context  as text 
-- moveFieldMeta2Context def k pan  ct = defFieldText k v ct 
--     where   v =   getTextFromYaml5 def k pan  :: Text                 

-- moveFieldMetaVal2Context ::  Text ->  Text -> Pandoc ->Meta -> Meta 
-- -- get a field from the metadata and put it in the context as metavalue 
-- moveFieldMetaVal2Context def k pan  ct = defField k v ct 
--     where   v =   getMetaValueFromYaml4 def k pan  :: MetaValue                

-- -- defField :: ToContext a b => Text -> b -> Context a -> Context a
 
test_map2list = assertEqual [] $ toList (unMeta . getMeta $ pandocA)
test_map2listadd = assertEqual [] $ toList . unMeta $ Meta (fromList [(("body"::Text), (MetaString "xx"))]) <> (getMeta pandocA)

md2Meta :: Bool -> MarkdownText -> ErrIO Meta 
-- convert a markdown file to MetaValue
md2Meta debug mdtext = do 
    pd@(Pandoc m1 p1) <- readMarkdown2 mdtext
    -- putIOwords ["pd \n", showT pd, "\n--"] 
    let c0 = zero :: Meta  
    let c1 = Meta (fromList [(("body"::Text), (MetaBlocks p1))]) <> m1  
    putIOwords ["cn", showT c1]
    -- todo 
    let    cn = c0
    return cn 

meta2res :: Bool -> Meta -> ErrIO (HTMLout, Latex)
-- the second part resulting in the two output files 
meta2res debug context = do 
    -- todo 
    return (zero, zero)

convertFull :: Bool -> Path Abs File -> ErrIO (HTMLout, Latex)
-- convert a md file to the html and latex format
convertFull debug fnin = do
    mdfile <- read8 fnin markdownFileType 
    context <- md2Meta debug mdfile 
    (h,l) <- meta2res debug context
    return (h,l)

fnA = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/startValues/A.md"
fnres_html =  makeAbsFile "/home/frank/tests/testhtmlA"
fnres_latex =  makeAbsFile "/home/frank/tests/testlatexA"

test_A = do 
    res1 <- runErr $ do 
        (hout, lout) <- convertFull False fnA 

        write8   fnres_html htmloutFileType hout
        write8   fnres_latex texFileType lout

        return "A"    
    -- let Right (target3, res3) = res5
    assertEqual (Right "") res1


        -- mdfile <- read8 fnA markdownFileType 
        -- pd@(Pandoc m1 p1) <- readMarkdown2 mdfile

        -- putIOwords ["pd \n", showT pd, "\n--"]

        -- -- get the metadata 
        -- let contextFromMeta = fillMeta2context pd (mempty:: Meta)
        -- let contextFromMetaVal =     -- the ones to preserve the format
        --         moveFieldMetaVal2Context "abstract missing" "abstract" pd
        --         . moveFieldMetaVal2Context "title missing" "title" pd
        --         . moveFieldMetaVal2Context "author missing" "author" pd
        --         $ contextFromMeta :: Meta
        -- let contextBasic = fillContext1 fnA  contextFromMetaVal:: Meta 

        -- contentHtml :: Text <- writeHtml5String2 pd   -- adds the content 
        -- contentTex :: Text <- writeTexSnip2 pd

        -- let ctHtml = fillContextHtml contentHtml contextBasic :: Meta
        --     ctLatex = fillContextLatex contentTex contextBasic :: Meta 
-------  break 1st 2nd 
        -- putIOwords ["ctHtml", showT ctHtml]

        -- templH :: Template Text <- compileDefaultTempalteHTML 
        -- templL :: Template Text  <-compileDefaultTempalteLatex
        -- -- renderTemplate :: (TemplateTarget a, ToContext a b) => Template a -> b -> Doc a
        -- let restplH = renderTemplate templH ctHtml :: Doc Text
        -- let resH = render (Just 50) restplH  :: Text  -- line length, can be Nothing
        -- let restplL = renderTemplate templL ctLatex :: Doc Text
        -- let resL = render (Just 50) restplL  :: Text  -- line length, can be Nothing
        
        -- putIOwords ["resH", resH]

    

pandocA = Pandoc 
    (Meta {unMeta = fromList [
        ("abstract",MetaInlines [Str "An",Space,Emph [Str "abstract"],Space,Str "for",Space,Str "the",Space,Strong [Str "example"],Space,Str "A"]),
        ("date",MetaInlines [Str "2020-06-16"]),
        ("keywords",MetaInlines [Str "A_KEYword"]),
        ("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title",Space,Str "of",Space,Str "A"])]}) 
    [Header 1 ("hl1_text-for-title-a",[],[]) 
        [Str "hl1_text",Space,Emph [Str "For"],Space,Strong [Str "Title"],Space,Str "A"],
        Para [Str "Nonsense",Space,Str "list."],
        BulletList [
                [Plain [Str "one"]],
                [Plain [Str "two"]]
        ]
    ] 