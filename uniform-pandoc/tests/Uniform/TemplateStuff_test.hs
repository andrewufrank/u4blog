---------------------------------------------------------------------------
--
-- Module      :  pandoc test
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

module Uniform.TemplateStuff_test where

import Test.Framework
    ( TestSuite, assertEqual, makeLoc, makeTestSuite, makeUnitTest )
-- import qualified Data.Map as M 
---- using uniform:
import Uniform.Json ()
import Uniform.PandocImports ( Pandoc(Pandoc), unPandocM ) 
import Uniform.Markdown () 
import Uniform.TexWriter ( writeTexSnip2 )
import Uniform.PandocHTMLwriter ( writeHtml5String2 )
import Text.Pandoc as Pandoc 
 
import Text.Pandoc.Definition
    ( nullMeta, Block(Plain), Inline(Str, Emph, Space, Strong) )
import Text.Pandoc.Writers ()
import Text.Pandoc.Writers.Shared ( defField ) 
import Text.DocTemplates as DocTemplates
    ( renderTemplate, Doc, Context )
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
import Uniform.TemplateStuff
-- import Uniform.Error           hiding (  (<.>)  )  -- (</>)
import UniformBase
import Uniform.HttpFiles
import Uniform.TexFileTypes

----- from 0.1.6.2
-- test_templ_html = do 
--     res1 <- runErr $ do 
--         htpl <- getDefaultTemplateHTML 
--         -- putIOwords ["htpl2 \n", htpl]
--         return "htpl"
--     -- let Right (target3, res3) = res5
--     assertEqual (Right "htpl") res1

-- test_templ_latex = do 
--     res1 <- runErr $ do 
--         htpl <- getDefaultTemplateLatex 
--         -- putIOwords ["htpl2 \n", htpl]
--         return "htpl"
--     -- let Right (target3, res3) = res5
--     assertEqual (Right "htpl") res1

-- fnres_html =  makeAbsFile "/home/frank/tests/testhtml1"

-- test_templ_comp_html = do 
--     res1 <- runErr $ do 
--         htpl2 <- unPandocM $ compileDefaultTemplate "html"
--         let cont1 = defField "abstract" ("A1"::Text) mempty :: Context Text 
--         let cont2 = defField "fontsize" ("12pt" :: Text)
--                     .  defField "documentclass" ("article"::Doc Text) 
--                     . defField "title" ("T1" :: Text) $ cont1  :: Context Text
--         -- putIOwords ["cont2", showT cont2 ]           
--         let tpl1 = renderTemplate htpl2 cont2  :: Doc Text
--         -- putIOwords ["tpl1 \n", showT tpl1]
--         let res1 = render (Just 50) tpl1  -- line length, can be Nothing

--         -- putIOwords ["res1 \n", showT res1]
--         let resHtml = HTMLout res1 
--         write8   fnres_html htmloutFileType resHtml
--         return "template"
--     -- let Right (target3, res3) = res5
--     assertEqual (Right "template") res1

-- fnres_latex =  makeAbsFile "/home/frank/tests/testlatex1"

-- test_templ_comp_latex = do 
--     res1 <- runErr $ do 
--         htpl2 <- unPandocM $ compileDefaultTemplate "latex"
--         let cont1 = defField "abstract" ("A1"::Text) mempty :: Context Text 
--         let cont2 = defField "title" ("T1" :: Text) cont1  :: Context Text
--         let cont2 = defField "fontsize" ("12pt" :: Text)
--                     .  defField "documentclass" ("article"::Doc Text) 
--                     . defField "title" ("T1" :: Text) $ cont1  :: Context Text
--         -- putIOwords ["latex cont2", showT cont2 ]           
--         let tpl1 = renderTemplate htpl2 cont2  :: Doc Text
--         -- putIOwords ["tpl1 \n", showT tpl1]
--         let res1 = render (Just 50) tpl1  -- line length, can be Nothing

--         -- putIOwords ["res1 \n", showT res1]
--         let reslatex = Latex res1 
--         write8   fnres_latex texFileType reslatex
--         return "template"
--     -- let Right (target3, res3) = res5
--     assertEqual (Right "template") res1





------------examples with a generic md file
fnminilatex =  makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/resources/minimalLatex.dtpl"
fnminihtml = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/resources/minimalHTML.dtpl"
metaOnlyText = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/resources/metaOnlyText.dtpl"
fnminires =  makeAbsFile "/home/frank/tests/testmini"

-- blogA =  meta2xx writeText resAWithBody  :: _
writeToMarkdown  pan1= unPandocM $ writeMarkdown Pandoc.def pan1

test_templ_comp_minihtml :: IO ()
test_templ_comp_minihtml = do 
    res1 <- runErr $ do 
        htpl2 <- compileTemplateFile2 metaOnlyText  
        blogA <- meta2xx writeToMarkdown resAWithBody1
        let tpl1 = renderTemplate htpl2 blogA  :: Doc Text
        -- putIOwords ["tpl1 \n", showT tpl1]
        let res1 = render (Just 50) tpl1  -- line length, can be Nothing

        putIOwords ["res1 \n",  res1]
        -- write8   fnminires htmloutFileType res1
        return res1
    assertEqual (Right resAprint) res1

resAprint=  "\n    \n-- from YAML header\n    title: title02 missing\n    abstract: abstract02 missing\n    keywords: one, two, three \n    version: publish\n    date: 2023-03-31 \n    body:  # 02-hl1title for 02 but missing\n\n02-text: The text for 02:  \n-- from Defaults   \n   def1: def1v  \n "



-- "\n    <!doctype html>\n    <html>\n    <head>\n    <title>T1</title>\n    <meta name=\"description\" content=A1>\n    <meta name=\"keywords\" content=>\n    </head>\n    <body>\n   title: T1 \n   version:  \n   date: \n\n   body: \n    </body>\n    </html>\n"
 
-- block1 :: Block 
-- block1 = Plain [Str "hl1_text",Space,Emph [Str "For"],Space,Strong [Str "Title"]]
-- -- block1 = Plain [Str "long", Space, Str "abstract"]

-- test_ast2md = do
--     res1 <- runErr $ do 
--         r <- writeAST3md def (Pandoc nullMeta [block1])
--         -- putIOwords ["abs1 as md", showT r]
--         return r
--     assertEqual (Right mdans2) res1
-- mdans1 = "long abstract\n"
-- mdans2 = "hl1\\_text *For* **Title**\n"
-- test_ast2html = do
--     res1 <- runErr $ do 
--         -- r :: _ <- writeAST3html def (Pandoc nullMeta [block1])
--         r :: Text <- writeHtml5String2 (Pandoc nullMeta [block1])
--         -- let r2 = render Nothing r
--         -- putIOwords ["abs1 as md",  r]
--         return r
--     assertEqual (Right hans2) res1
-- hans1 = "long abstract"
-- hans2 = "hl1_text <em>For</em> <strong>Title</strong>"

-- test_html = do
--     res1 <- runErr $ do 
--         -- r :: _ <- writeAST3html def (Pandoc nullMeta [block1])
--         r :: Text <- writeHtml5String2 (Pandoc nullMeta [block1])
--         -- let r2 = render Nothing r
--         -- putIOwords ["abs1 as html",  r]
--         return r
--     assertEqual (Right hans2) res1

-- test_tex = do
--     res1 <- runErr $ do 
--         -- r :: _ <- writeAST3html def (Pandoc nullMeta [block1])
--         r :: Text <- writeTexSnip2 (Pandoc nullMeta [block1])
--         -- let r2 = render Nothing r
--         -- putIOwords ["abs1 as tex",  r]
--         return r
--     assertEqual (Right tans2 ) res1
-- tans2 =  "hl1\\_text \\emph{For} \\textbf{Title}"
--         -- -- htpl2 <- compileTemplateFile False htpl

-- -- does only look at the block, not using the header
-- test_texsnip1 = do 
--     res1 <- runErr $ do 
--         tex1 <- writeTexSnip2 pandocY
--         putIOwords ["tex1 \n", tex1]
--         return tex1
--     -- let Right (target3, res3) = res5
--     assertEqual (Right zero) res1
--         -- gives
--         -- tex1 
--         --  \hypertarget{hl1_text}{%
--         -- \section{hl1\_text}\label{hl1_text}}

--         -- Nonsense sentence.

-- -- produce html 
-- -- with my old code:



-- -- get a string (Text)
-- test_htmlString = do 
--     res1 <- runErr . unPandocM $ do 
--         html1 :: Text <- writeHtml5String html5Options pandocY
--         -- tex1 <- writeLaTeX2 pandocY
--         putIOwords ["html1 \n", html1]
--         return html1
--     -- let Right (target3, res3) = res5
--     assertEqual (Right zero) res1
--         -- gives
--         --  <h1 id="hl1_text">hl1_text</h1>
--         -- <p>Nonsense sentence.</p>

-- -- shows blaze result
-- test_html = do 
--     res1 <- runErr . unPandocM $ do 
--         html1  <- writeHtml5  html5Options pandocY
--         let t1 = s2t . unpack $ renderHtml html1 :: Text
--         -- tex1 <- writeLaTeX2 pandocY
--         putIOwords ["html1 \n",  t1]
--         return t1
--     -- let Right (target3, res3) = res5
--     assertEqual (Right zero) res1
--             -- gives same as before
--             -- html1 
--             -- <h1 id="hl1_text">hl1_text</h1>
--             -- <p>Nonsense sentence.</p>


-- for tex

-- -- produces texsnip
-- test_tex1 = do 
--     res1 <- runErr $ do 
--         tex1 <- writeLaTeX2 pandocY
--         return tex1
--     -- let Right (target3, res3) = res5
--     assertEqual (Right zero) res1

-- -- what is different to tex1?
-- test_latex1 = do 
--     res1 <- runErr $ do 
--         tex1 <- writeLaTeX2 pandocY
--         return tex1
--     -- let Right (target3, res3) = res5
--     assertEqual (Right zero) res1

-- -- try to produce a standalone latex Tex file - not working!
-- test_latex2 = do 
--     res1 <- runErr .  unPandocM $ do 
--         tpl <-   compileDefaultTemplate "latex"
--         let doc1 =  renderTemplate tpl (getMeta pandocY)
--         let doc2 = render Nothing doc1
--         return doc2
--     -- let Right (target3, res3) = res5
--     assertEqual (Right zero) res1



