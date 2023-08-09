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



