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


test_templ_html = do 
    res1 <- runErr $ do 
        htpl <- getDefaultTemplateHTML 
         
        putIOwords ["htpl2 \n", htpl]
        return "htpl"
    -- let Right (target3, res3) = res5
    assertEqual (Right "htpl") res1

fn2 =  makeAbsFile "/home/frank/testhtml1"

test_templ_comp_html = do 
    res1 <- runErr $ do 
        htpl2 <- unPandocM $ compileDefaultTemplate "html"
        let cont1 = defField "abstract" ("A1"::Text) mempty :: Context Text 
        let cont2 = defField "title" ("T1" :: Text) cont1  :: Context Text
        let tpl1 = renderTemplate htpl2 cont2  :: Doc Text
        let res1 = render (Just 50) tpl1  -- line length, can be Nothing

        putIOwords ["res1 \n", showT res1]
        let resHtml = HTMLout res1 
        write8   fn2 htmloutFileType resHtml
        return "template"
    -- let Right (target3, res3) = res5
    assertEqual (Right "template") res1

 
        -- -- htpl2 <- compileTemplateFile False htpl

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



