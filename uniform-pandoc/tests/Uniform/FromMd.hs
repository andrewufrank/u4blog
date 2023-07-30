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
-- {-# LANGUAGE DeriveGeneric          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.FromMd where

import Test.Framework
import qualified Data.Map as M
import Data.Map (fromList, toList)
---- using uniform:
import Uniform.Json hiding (toList, fromList)
import Uniform.PandocImports
import Uniform.Markdown
import Uniform.TexWriter ()
import Uniform.PandocHTMLwriter
import Text.Pandoc
import Text.Pandoc.Definition ()
import Text.Pandoc.Writers ()
import Text.Pandoc.Writers.Shared ()
import Text.DocTemplates as DocTemplates ( Doc )
import Text.DocLayout (render)
import Data.Text.Lazy (unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
-- import Data.Map 
-- import Uniform.Filenames 
-- import Uniform2.Filetypes4sites should not be imported here
import Uniform.Test.TestHarness ()
import Uniform.MetaStuff_test ()
-- import Uniform.Markdown_test 
import Uniform.MetaStuff
import Uniform.TemplatesStuff
-- import Uniform.Error           hiding (  (<.>)  )  -- (</>)
import UniformBase
import Uniform.HttpFiles
import Uniform.TexFileTypes

test_meta2text = do  
    res1 <- runErr $ do 
        meta2htmltext False (getMeta pandocA)
    assertEqual (Right zeromap) res1

res1a = fromList  -- the text, lost the styling, metaValueToText wrong 
     [("abstract", "An abstract for the example A"),
      ("date", "2020-06-16"), ("keywords", "A_KEYword"),
      ("title", "the real title of A")]
zeromap = fromList [("abstract",
       "An <em>abstract</em> for the <strong>example</strong> A"),
      ("date", "2020-06-16"), ("keywords", "A_KEYword"),
      ("title", "the <strong>real</strong> title of A")] :: M.Map Text Text 

test_step1 = do 
    res1 <- runErr $ do 
        md <- read8 fnA markdownFileType
        md2Meta False md 
    assertEqual (Right metaStep1) res1 

metaStep1 = 
 Meta {unMeta = fromList [("abstract",MetaInlines [Str "An",Space,Emph [Str "abstract"],Space,Str "for",Space,Str "the",Space,Strong [Str "example"],Space,Str "A"]),("body",MetaBlocks [Header 1 ("hl1_text-for-title-a",[],[]) [Str "hl1_text",Space,Emph [Str "For"],Space,Strong [Str "Title"],Space,Str "A"],Para [Str "Nonsense",Space,Str "list."],BulletList [[Plain [Str "one"]],[Plain [Str "two"]]]]),("date",MetaInlines [Str "2020-06-16"]),("keywords",MetaInlines [Str "A_KEYword"]),("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title",Space,Str "of",Space,Str "A"])]} 




htmlStep1 = fromList [("abstract",
       "An <em>abstract</em> for the <strong>example</strong> A"),
      ("body",
       "<h1 id=\"hl1_text-for-title-a\">hl1_text <em>For</em>\n<strong>Title</strong> A</h1>\n<p>Nonsense list.</p>\n<ul>\n<li>one</li>\n<li>two</li>\n</ul>"),
      ("date", "2020-06-16"), ("keywords", "A_KEYword"),
      ("title", "the <strong>real</strong> title of A")] :: M.Map Text Text
test_htmltext = do 
    res1 <- runErr $ do 
        meta2htmltext False metaStep1 
    assertEqual (Right htmlStep1) res1 



meta2hres :: Bool -> Meta -> ErrIO HTMLout
-- step2: the second part resulting in HTML result
meta2hres debug meta = do
    putIOwords ["meta2hres meta \n", showT meta, "\n--"]
    -- convert to list of (text,Block) 
    -- make M.Map and pass to render template 

    tHtml :: M.Map Text Text <- meta2htmltext debug meta
    putIOwords ["meta2hres tHtml \n", showT tHtml, "\n--"]

    templH :: Template Text <- compileDefaultTempalteHTML
        -- templL :: Template Text  <-compileDefaultTempalteLatex
        -- -- renderTemplate :: (TemplateTarget a, ToContext a b) => Template a -> b -> Doc a
    let restplH = renderTemplate templH tHtml :: Doc Text
    let resH = render (Just 50) restplH  :: Text  -- line length, can be Nothing
        -- let restplL = renderTemplate templL ctLatex :: Doc Text
        -- let resL = render (Just 50) restplL  :: Text  -- line length, can be Nothing    -- todo 
    return (HTMLout resH)


convertFull :: Bool -> Path Abs File -> ErrIO (HTMLout, Latex)
-- convert a md file to the html and latex format
convertFull debug fnin = do
    mdfile <- read8 fnin markdownFileType
    context <- md2Meta debug mdfile
    h <- meta2hres debug context
    return (h,zero)



---------- the test to produce the output files html and latex 
fnA :: Path Abs File
fnA = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/startValues/A.md"
fnres_html :: Path Abs File
fnres_html =  makeAbsFile "/home/frank/tests/testhtmlA"
fnres_latex :: Path Abs File
fnres_latex =  makeAbsFile "/home/frank/tests/testlatexA"

test_A :: IO ()
test_A = do
    res1 <- runErr $ do
        (hout, lout) <- convertFull False fnA

        write8   fnres_html htmloutFileType hout
        write8   fnres_latex texFileType lout

        return "A"
    -- let Right (target3, res3) = res5
    assertEqual (Right "A") res1  -- todo is fake 


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



pandocA :: Pandoc
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