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

module Uniform.Pandoc_test where

import Test.Framework
-- import qualified Data.Map as M 
---- using uniform:
import Uniform.Json hiding (fromList)
import Uniform.PandocImports 
import Uniform.Markdown 
import Uniform.TexWriter
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Writers
import Text.DocTemplates as DocTemplates
import Text.DocLayout (render)
import Data.Map 
-- import Uniform.Filenames 
-- import Uniform2.Filetypes4sites should not be imported here

import Uniform.Test.TestHarness
-- import Uniform.Markdown_test 
-- import Uniform.Error           hiding (  (<.>)  )  -- (</>)
import UniformBase
import Text.DocLayout (render)
import Text.DocTemplates as DocTemplates

-- test getFromYaml value on abstract
test_ka = assertEqual (Just abs1) $ getFromYaml "abstract" pandocY
-- test getFromYaml with default as metavlaue, but value is present
test_fa = assertEqual abs1 $
             getTextFromYaml4 "oneAbstract" "abstract" pandocY
-- get as Text 
test_faT = assertEqual "The long struggle" $
             getTextFromYaml5 "oneAbstract" "abstract" pandocY


abs1:: MetaValue
abs1 =  MetaInlines [Str "The", Space, Str "long", Space, Str "struggle"]

-- check conversion of metavalue to text 
test_mvt = assertEqual abs1t $ metaValueToText abs1

abs1t = Just "The long struggle"
fn1 =  makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/startValues/someTextWithYAML.md"

-- ad a value to meta
key1 = "indexEntry" :: Text 
test_addMeta = assertEqual testval1 $
            getTextFromYaml5 "def" key1 .
            meta2pandoc .    addMetaField2 key1 (testval1 :: Text) $ metaY 

-- check that the value is stored and can be retrieved
testval1 = "A test Text value" :: Text 
test_addPandoc = assertEqual (Just . MetaString $ testval1) $ 
            getFromYaml key1 . addMetaField2pandoc key1 testval1 $ pandocY

-- does only look at the block, not using the header
test_texsnip1 = do 
    res1 <- runErr $ do 
        tex1 <- writeTexSnip2 pandocY
        return tex1
    -- let Right (target3, res3) = res5
    assertEqual (Right zero) res1

-- produces texsnip
test_tex1 = do 
    res1 <- runErr $ do 
        tex1 <- writeLaTeX2 pandocY
        return tex1
    -- let Right (target3, res3) = res5
    assertEqual (Right zero) res1

-- what is different to tex1?
test_latex1 = do 
    res1 <- runErr $ do 
        tex1 <- writeLaTeX2 pandocY
        return tex1
    -- let Right (target3, res3) = res5
    assertEqual (Right zero) res1

-- try to produce a standalone latex Tex file - not working!
test_latex2 = do 
    res1 <- runErr .  unPandocM $ do 
        tpl <-   compileDefaultTemplate "latex"
        let doc1 =  renderTemplate tpl (getMeta pandocY)
        let doc2 = render Nothing doc1
        return doc2
    -- let Right (target3, res3) = res5
    assertEqual (Right zero) res1

test_readmd = do 
    res1 <- runErr $ do 
        mdfile <- read8 fn1 markdownFileType 
        pd <- readMarkdown2 mdfile
        return True
    -- let Right (target3, res3) = res5
    assertEqual (Right True) res1



metaY :: Meta 
metaY = Meta {unMeta = fromList [("abstract",MetaInlines [Str "The",Space,Str "long",Space,Str "struggle"]),("date",MetaInlines [Str "2020-06-16"]),("keywords",MetaInlines [Str "Haskell",Space,Str "IDE"]),("title",MetaInlines [Str "a",Space,Str "new",Space,Str "start"])]}

pandocY = Pandoc (Meta {unMeta = fromList 
    [("abstract",MetaInlines [Str "The",Space,Str "long",Space,Str "struggle"]),("date",MetaInlines [Str "2020-06-16"]),("keywords",MetaInlines [Str "Haskell",Space,Str "IDE"]),("title",MetaInlines [Str "a",Space,Str "new",Space,Str "start"])]})
     [Header 1 ("hl1_text",[],[]) [Str "hl1_text"],
        Para [Str "Nonsense",Space,Str "sentence."]
    -- , Header 1 ("gives",[],[]) [Str "gives"],Para [Str "$someTextWithYAML.md",Space,Str "Meta",Space,Str "{unMeta",Space,Str "=",Space,Str "fromList",Space,Str "[(",Quoted DoubleQuote [Str "abstract"],Str ",MetaInlines",Space,Str "[Str",Space,Quoted DoubleQuote [Str "The"],Str ",Space,Str",Space,Quoted DoubleQuote [Str "long"],Str ",Space,Str",Space,Quoted DoubleQuote [Str "struggle"],Str "]),(",Quoted DoubleQuote [Str "date"],Str ",MetaInlines",Space,Str "[Str",Space,Quoted DoubleQuote [Str "2020-06-16"],Str "]),(",Quoted DoubleQuote [Str "keywords"],Str ",MetaInlines",Space,Str "[Str",Space,Quoted DoubleQuote [Str "Haskell"],Str ",Space,Str",Space,Quoted DoubleQuote [Str "IDE"],Str "]),(",Quoted DoubleQuote [Str "title"],Str ",MetaInlines",Space,Str "[Str",Space,Quoted DoubleQuote [Str "a"],Str ",Space,Str",Space,Quoted DoubleQuote [Str "new"],Str ",Space,Str",Space,Quoted DoubleQuote [Str "start"],Str "])]}"
    ]
    
