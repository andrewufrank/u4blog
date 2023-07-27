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

module Uniform.MetaStuff_test where

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
import Text.DocTemplates as DocTemplates
import Text.DocLayout (render)
import Data.Text.Lazy (unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Map 
-- import Uniform.Filenames 
-- import Uniform2.Filetypes4sites should not be imported here

import Uniform.Test.TestHarness
import Uniform.MetaStuff
-- import Uniform.Markdown_test 
-- import Uniform.Error           hiding (  (<.>)  )  -- (</>)
import UniformBase
import Text.DocLayout (render)
import Text.DocTemplates as DocTemplates

-- test getFromYaml value on abstract
test_ka = assertEqual (Just abs1) $ getFromYaml "abstract" pandocY
-- test getFromYaml with default as metavlaue, but value is present
test_fa = assertEqual abs1 $
             getMetaValueFromYaml4 "oneAbstract" "abstract" pandocY
-- get as Text 
test_faT = assertEqual "long abstract" $
             getTextFromYaml5 "oneAbstract" "abstract" pandocY

abs1:: MetaValue
abs1 =  MetaInlines [Str "long", Space, Str "abstract"]

-- check conversion of metavalue to text 
test_mvt = assertEqual abs1t $ metaValueToText abs1

abs1t = Just "long abstract"
-- ad a value to meta
key1 = "indexEntry" :: Text 
test_addMeta = assertEqual testval1 $
            getTextFromYaml5 "def" key1 .
            meta2pandoc .    addMetaField2 key1 (testval1 :: Text) $ metaY 

-- check that the value is stored and can be retrieved
testval1 = "A test Text value" :: Text 
test_addPandoc = assertEqual (Just . MetaString $ testval1) $ 
            getFromYaml key1 . addMetaField2pandoc key1 testval1 $ pandocY

-- basics to get the data 
fn1 =  makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/startValues/someTextWithYAML.md"
-- -- uniform-pandoc/tests/data/startValues/someTextWithYAML.md

test_readmd = do 
    res1 <- runErr $ do 
        mdfile <- read8 fn1 markdownFileType 
        pd <- readMarkdown2 mdfile
        -- putIOwords ["pd \n", showT pd, "\n--"]
        return True
    assertEqual (Right True) res1   -- set to False to produce output

metaY :: Meta 
metaY = Meta {unMeta = fromList 
    [("abstract",MetaInlines [Str "The",Space,Str "long",Space,Str "struggle"]),("date",MetaInlines [Str "2020-06-16"])
    ,("keywords",MetaInlines [Str "Haskell",Space,Str "IDE"]),("title",MetaInlines [Str "a",Space,Str "new",Space,Str "start"])
    ]}
 
pandocY :: Pandoc 
pandocY = Pandoc (Meta {unMeta = fromList [("abstract",MetaInlines [Str "long",Space,Str "abstract"]),("date",MetaInlines [Str "2020-06-16"]),("keywords",MetaInlines [Str "KEYword"]),("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title"])]}) [Header 1 ("hl1_text-for-title",[],[]) [Str "hl1_text",Space,Emph [Str "For"],Space,Strong [Str "Title"]],Para [Str "Nonsense",Space,Str "list."],BulletList [[Plain [Str "one"]],[Plain [Str "two"]]]] 
 