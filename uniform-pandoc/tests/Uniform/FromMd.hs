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
import Uniform.Json hiding (toList, fromList)
import Uniform.PandocImports
import Uniform.Markdown
import Uniform.PandocHTMLwriter
import Text.Pandoc
import Text.DocTemplates as DocTemplates ( Doc )
import Text.DocLayout (render)
import Data.Text.Lazy (unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Uniform.MetaStuff
import Uniform.TemplateStuff
import Uniform.TexWriter
import UniformBase
-- import Uniform.FileIO
import Uniform.HttpFiles
import Uniform.TexFileTypes
import Text.Pandoc.Shared (addMetaField)

----------- for experiment 0.1.6.3 
stFn = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/startValues/A.md"

fortest_step1 fnA resA= do 
    res1 <- runErr $ do 
        md <- read8 fnA markdownFileType
        md2Meta fnA md 
    assertEqual (Right resA) res1 

test_ok = fortest_step1 stFn resA
resA = Meta{unMeta =
          fromList
            [("abstract", MetaInlines [Str "AnAbstract"]),
             ("body", MetaBlocks [Header 1 ("atitle", [], []) [Str "aTitle"]]),
             ("title", MetaInlines [Str "theTitle"])]}

stAfail = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/startValues/Afail.md"
test_fail = fortest_step1 stAfail resA

----------- from 0.1.6.2

-- latexRes = fromList 
--     [("abstract", "An \\emph{abstract} for the \\textbf{example} A"),
--       ("date", "2020-06-16"), ("keywords", "A\\_KEYword"),
--       ("title", "the \\textbf{real} title of A")]
-- test_meta2latex = do  
--     res1 <- runErr $ do 
--         meta2xx  writeTexSnip2 (getMeta pandocA)
--     assertEqual (Right latexRes) res1

-- test_meta2htmltext = do  
--     res1 <- runErr $ do 
--         meta2xx  writeHtml5String2 (getMeta pandocA)
--     assertEqual (Right htmlRes) res1

-- res1a = fromList  -- the text, lost the styling, metaValueToText wrong 
--      [("abstract", "An abstract for the example A"),
--       ("date", "2020-06-16"), ("keywords", "A_KEYword"),
--       ("title", "the real title of A")]
-- htmlRes = fromList [("abstract",
--        "An <em>abstract</em> for the <strong>example</strong> A"),
--       ("date", "2020-06-16"), ("keywords", "A_KEYword"),
--       ("title", "the <strong>real</strong> title of A")] :: M.Map Text Text 

-- test_step1 = do 
--     res1 <- runErr $ do 
--         md <- read8 fnA markdownFileType
--         md2Meta fnA md 
--     assertEqual (Right metaStep1) res1 

-- metaStep1 = 
--  Meta {unMeta = fromList [("abstract",MetaInlines [Str "An",Space,Emph [Str "abstract"],Space,Str "for",Space,Str "the",Space,Strong [Str "example"],Space,Str "A"]),("body",MetaBlocks [Header 1 ("hl1_text-for-title-a",[],[]) [Str "hl1_text",Space,Emph [Str "For"],Space,Strong [Str "Title"],Space,Str "A"],Para [Str "Nonsense",Space,Str "list."],BulletList [[Plain [Str "one"]],[Plain [Str "two"]]]]),("date",MetaInlines [Str "2020-06-16"]),("keywords",MetaInlines [Str "A_KEYword"]),("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title",Space,Str "of",Space,Str "A"])]} 




-- htmlStep1 = fromList [("abstract",
--        "An <em>abstract</em> for the <strong>example</strong> A"),
--       ("body",
--        "<h1 id=\"hl1_text-for-title-a\">hl1_text <em>For</em>\n<strong>Title</strong> A</h1>\n<p>Nonsense list.</p>\n<ul>\n<li>one</li>\n<li>two</li>\n</ul>"),
--       ("date", "2020-06-16"), ("keywords", "A_KEYword"),
--       ("title", "the <strong>real</strong> title of A")] :: M.Map Text Text
-- test_htmltext = do 
--     res1 <- runErr $ do 
--         meta2xx  writeHtml5String2 metaStep1 
--     assertEqual (Right htmlStep1) res1 



-- fnLatexTempl :: Path Abs File
-- fnLatexTempl = makeAbsFile "/home/frank/Workspace11/dainoTheme/templates/latexTufte81.dtpl"
-- fnHtmlTempl = makeAbsFile "/home/frank/Workspace11/dainoTheme/templates/master7tufte.dtpl"

-- convertFullwithTemplate ::  Path Abs File -> ErrIO (HTMLout, Latex)
-- -- convert a md file to the html and latex format
-- convertFullwithTemplate  fnin = do
--     mdfile <- read8 fnin markdownFileType
--     context <- md2Meta fnin mdfile

--     latexTempl <- compileTemplateFile2 fnLatexTempl
--     htmlTempl  <- compileTemplateFile2 fnHtmlTempl

--     -- htmlTempl <- compileDefaultTempalteHTML
--     -- latexTempl <- compileDefaultTempalteLatex

--     h <- meta2hres  htmlTempl context

--     l <- meta2latex  latexTempl context
--     return (h,l)

-- convertFull ::  Path Abs File -> ErrIO (HTMLout, Latex)
-- -- convert a md file to the html and latex format
-- convertFull  fnin = do
--     mdfile <- read8 fnin markdownFileType
--     context <- md2Meta fnin mdfile

--     htmlTempl <- compileDefaultTempalteHTML
--     latexTempl <- compileDefaultTempalteLatex

--     h <- meta2hres  htmlTempl context

--     l <- meta2latex  latexTempl context
--     return (h,l)



-- ---------- the test to produce the output files html and latex 
-- fnA :: Path Abs File
-- fnA = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/startValues/A.md"
-- fnres_html :: Path Abs File
-- fnres_html =  makeAbsFile "/home/frank/tests/testhtmlA"
-- fnres_latex :: Path Abs File
-- fnres_latex =  makeAbsFile "/home/frank/tests/testlatexA"

-- test_A :: IO ()
-- test_A = do
--     res1 <- runErr $ do
--         -- (hout, lout) <- convertFull fnA
--         (hout, lout) <- convertFullwithTemplate fnA

--         write8   fnres_html htmloutFileType hout
--         write8   fnres_latex texFileType lout

--         return "A"
--     -- let Right (target3, res3) = res5
--     assertEqual (Right "A") res1  -- todo is fake 


--         -- mdfile <- read8 fnA markdownFileType 
--         -- pd@(Pandoc m1 p1) <- readMarkdown2 mdfile

--         -- putIOwords ["pd \n", showT pd, "\n--"]

--         -- -- get the metadata 
--         -- let contextFromMeta = fillMeta2context pd (mempty:: Meta)
--         -- let contextFromMetaVal =     -- the ones to preserve the format
--         --         moveFieldMetaVal2Context "abstract missing" "abstract" pd
--         --         . moveFieldMetaVal2Context "title missing" "title" pd
--         --         . moveFieldMetaVal2Context "author missing" "author" pd
--         --         $ contextFromMeta :: Meta
--         -- let contextBasic = fillContext1 fnA  contextFromMetaVal:: Meta 

--         -- contentHtml :: Text <- writeHtml5String2 pd   -- adds the content 
--         -- contentTex :: Text <- writeTexSnip2 pd

--         -- let ctHtml = fillContextHtml contentHtml contextBasic :: Meta
--         --     ctLatex = fillContextLatex contentTex contextBasic :: Meta 
-- -------  break 1st 2nd 
--         -- putIOwords ["ctHtml", showT ctHtml]

--         -- templH :: Template Text <- compileDefaultTempalteHTML 
--         -- templL :: Template Text  <-compileDefaultTempalteLatex
--         -- -- renderTemplate :: (TemplateTarget a, ToContext a b) => Template a -> b -> Doc a
--         -- let restplH = renderTemplate templH ctHtml :: Doc Text
--         -- let resH = render (Just 50) restplH  :: Text  -- line length, can be Nothing
--         -- let restplL = renderTemplate templL ctLatex :: Doc Text
--         -- let resL = render (Just 50) restplL  :: Text  -- line length, can be Nothing

--         -- putIOwords ["resH", resH]



-- pandocA :: Pandoc
-- pandocA = Pandoc
--     (Meta {unMeta = fromList [
--         ("abstract",MetaInlines [Str "An",Space,Emph [Str "abstract"],Space,Str "for",Space,Str "the",Space,Strong [Str "example"],Space,Str "A"]),
--         ("date",MetaInlines [Str "2020-06-16"]),
--         ("keywords",MetaInlines [Str "A_KEYword"]),
--         ("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title",Space,Str "of",Space,Str "A"])]})
--     [Header 1 ("hl1_text-for-title-a",[],[])
--         [Str "hl1_text",Space,Emph [Str "For"],Space,Strong [Str "Title"],Space,Str "A"],
--         Para [Str "Nonsense",Space,Str "list."],
--         BulletList [
--                 [Plain [Str "one"]],
--                 [Plain [Str "two"]]
--         ]
--     ]