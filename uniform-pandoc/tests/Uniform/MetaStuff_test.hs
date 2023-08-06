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
-- {-# LANGUAGE DeriveGeneric          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.MetaStuff_test where

import Test.Framework
-- import qualified Data.Map as M 
---- using uniform:
import Uniform.Json ()
import Uniform.PandocImports ( Meta(..), Pandoc(..) ) 
import Uniform.Markdown ( markdownFileType, readMarkdown2 ) 
import Uniform.TexWriter ()
import Uniform.PandocHTMLwriter  
import Text.Pandoc
    
import Text.Pandoc.Definition ()
import Text.Pandoc.Writers ()
import Text.DocTemplates as DocTemplates ()
import Text.DocLayout (render)
import Data.Text.Lazy (unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Map ( fromList, toList) 
-- import Uniform.Filenames 
-- import Uniform2.Filetypes4sites should not be imported here

import Uniform.Test.TestHarness ()
import Uniform.MetaStuff
 

-- import Uniform.Markdown_test 
-- import Uniform.Error           hiding (  (<.>)  )  -- (</>)
import UniformBase
-- import Text.DocLayout (render)
-- import Text.DocTemplates as DocTemplates
-- 
-- test getFromYaml value on abstract
test_ka = assertEqual (Just abs1) $ getFromYaml "abstract" pandocY
-- test getFromYaml with default as metavlaue, but value is present
test_fa = assertEqual abs1 $
             getMetaValueFromYaml4 "oneAbstract" "abstract" pandocY
-- get as Text 
test_faT = assertEqual "long abstract" $
             getTextFromYaml5 "oneAbstract" "abstract" pandocY


abs2 :: MetaValue
abs2r :: [Block]
abs2r = [Plain
     [Str "An", Space, Emph [Str "abstract"], Space, Str "for", Space,
      Str "the", Space, Strong [Str "example"], Space, Str "A"]]

abs2 = MetaInlines [Str "An",Space,Emph [Str "abstract"],Space,Str "for",Space,Str "the",Space,Strong [Str "example"],Space,Str "A"]
test_mvb = assertEqual (Just abs2r) $ metaValueToBlock abs2

abs1:: MetaValue
abs1 =  MetaInlines [Str "long", Space, Str "abstract"]


-- check conversion of metavalue to htmltext 
test_block_html :: IO ()
test_block_html = do 
    res1 <- runErr $ do 
        block2xx writeHtml5String2 abs2r
    assertEqual (Right abs2html) $ res1
abs2html = "An <em>abstract</em> for the <strong>example</strong> A"

-- check conversion of metavalue to text 
test_mvt :: IO ()
test_mvt = assertEqual abs1t $ metaValueToText abs1

test_metaval_html :: IO ()
test_metaval_html = do 
    res1 <- runErr $ do 
        metaValue2xx writeHtml5String2 abs2
    assertEqual (Right abs2html) $ res1

    
abs1t :: Maybe Text
abs1t = Just "long abstract"
-- ad a value to meta
key1 :: Text
key1 = "indexEntry" :: Text 
test_addMeta :: IO ()
test_addMeta = assertEqual testval1 $
            getTextFromYaml5 "def" key1 .
            meta2pandoc .    addMetaFieldT key1 (testval1) $ metaY 

-- check that the value is stored and can be retrieved
testval1 :: Text
testval1 = "A test Text value" :: Text 
test_addPandoc :: IO ()
test_addPandoc = assertEqual (Just . MetaString $ testval1) $ 
            getFromYaml key1 . addMetaField2pandoc key1 testval1 $ pandocY


testval2 :: Text
testval2= "A test Text value" :: Text 
test_setgetmeta :: IO ()
test_setgetmeta = assertEqual (testval2) $ 
            flip getValue4meta key1 . setValue2meta key1 testval2 $ metaY



test_map2list :: IO ()
test_map2list = assertEqual [("body", MetaString "xx")] $ toList (unMeta meta1)
test_map2listadd :: IO ()
test_map2listadd = assertEqual cont12 $ toList . unMeta $ Meta (fromList [(("abst"::Text), (MetaString "yy"))]) <>  meta1

meta1 :: Meta
meta1 = Meta (fromList [(("body"::Text), (MetaString "xx"))])
cont12 :: [(Text, MetaValue)]
cont12 =  [("abst", MetaString "yy"), ("body", MetaString "xx")]

defs1 :: [(Text,Text)]
defs1 = [("def1","def1v"),("date","dataFalse")]
test_defs :: IO ()
test_defs = assertEqual defs1res $ addListOfDefaults defs1 metaY

defs1res :: Meta
defs1res = Meta{unMeta =
       fromList
         [("abstract",
           MetaInlines [Str "The", Space, Str "long", Space, Str "struggle"]),
          ("date", MetaInlines [Str "2020-06-16"]),
          ("def1", MetaString "def1v"),
          ("keywords", MetaInlines [Str "Haskell", Space, Str "IDE"]),
          ("title",
           MetaInlines [Str "a", Space, Str "new", Space, Str "start"])]}
--------------------------------------------------------------------------
-- basics to get the data 
fn1 :: Path Abs File
fn1 =  makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/startValues/someTextWithYAML.md"
-- -- uniform-pandoc/tests/data/startValues/someTextWithYAML.md

test_readmd :: IO ()
-- test with fn1 to show the pandoc 
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

-- meta with bold etc...

metaA = Meta {unMeta = fromList [
        ("abstract",MetaInlines [Str "An",Space,Emph [Str "abstract"],Space,Str "for",Space,Str "the",Space,Strong [Str "example"],Space,Str "A"]),
        ("date",MetaInlines [Str "2020-06-16"]),
        ("keywords",MetaInlines [Str "A_KEYword"]),
        ("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title",Space,Str "of",Space,Str "A"])]}
 
pandocY :: Pandoc 
pandocY = Pandoc (Meta {unMeta = fromList [("abstract",MetaInlines [Str "long",Space,Str "abstract"]),("date",MetaInlines [Str "2020-06-16"]),("keywords",MetaInlines [Str "KEYword"]),("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title"])]}) [Header 1 ("hl1_text-for-title",[],[]) [Str "hl1_text",Space,Emph [Str "For"],Space,Strong [Str "Title"]],Para [Str "Nonsense",Space,Str "list."],BulletList [[Plain [Str "one"]],[Plain [Str "two"]]]] 
 