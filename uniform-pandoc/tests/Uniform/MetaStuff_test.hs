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
import Uniform.PandocImports ( Meta(..), Pandoc(..), unPandocM )
import Uniform.Markdown ( markdownFileType, readMarkdown2 )
import Text.Pandoc as Pandoc
    ( Block(Plain, Header, Para, BulletList),
      Meta(..),
      MetaValue(MetaInlines, MetaBlocks, MetaString),
      Pandoc(..),
      writeNative,
      def,
      Inline(Str, Emph, Strong, Space) )

import Text.DocLayout (render)
import Data.Text.Lazy (unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Map as M
import Data.Map ( fromList, toList)
import Uniform.MetaStuff
    ( Meta(..), Pandoc(..), addListOfDefaults, md2Meta_Process )
import UniformBase


writeText pan1= unPandocM $ Pandoc.writeNative Pandoc.def pan1


defs1 :: [(Text,Text)]
defs1 = [("def1","def1v"),("date","dataFalse")] 
-- simulates the insert of defaults 
-- the date nmust not be overwritten from yaml value 
test_defs :: IO ()
test_defs = assertEqual resA1 $ Pandoc m2 b1
    where 
            (Pandoc m1 b1) = pandocA
            m2 = addListOfDefaults defs1  m1

resA1 :: Pandoc
resA1 =  Pandoc
  (Meta{unMeta =
          fromList
            [("abstract",
              MetaInlines [Str "abstract02", Space, Str "missing"]),
             ("date", MetaInlines [Str "2023-03-31"]),
             ("def1", MetaString "def1v"),
             ("keywords",
              MetaInlines [Str "one,", Space, Str "two,", Space, Str "three"]),
             ("title", MetaInlines [Str "title02", Space, Str "missing"]),
             ("version", MetaInlines [Str "publish"])]})
  [Header 1 ("02-hl1title-for-02-but-missing", [], [])
     [Str "02-hl1title", Space, Str "for", Space, Str "02", Space,
      Str "but", Space, Str "missing"],
   Para
     [Str "02-text:", Space, Str "The", Space, Str "text", Space,
      Str "for", Space, Str "02:"]]


-- process cites and put body into meta 
test_body = do
    res1 <- runErr $ md2Meta_Process resA1
    assertEqual (Right resAWithBody1) res1

resAWithBody1 :: Meta
resAWithBody1 = Meta{unMeta =
          fromList
           [("abstract",
              MetaInlines [Str "abstract02", Space, Str "missing"]),
             ("body",
              MetaBlocks
                [Header 1 ("02-hl1title-for-02-but-missing", [], [])
                   [Str "02-hl1title", Space, Str "for", Space, Str "02", Space,
                    Str "but", Space, Str "missing"],
                 Para
                   [Str "02-text:", Space, Str "The", Space, Str "text", Space,
                    Str "for", Space, Str "02:"]]),
             ("date", MetaInlines [Str "2023-03-31"]),
             ("def1", MetaString "def1v"),
             ("keywords",
              MetaInlines [Str "one,", Space, Str "two,", Space, Str "three"]),
             ("title", MetaInlines [Str "title02", Space, Str "missing"]),
             ("version", MetaInlines [Str "publish"])]}

resBWithBody1 = resAWithBody1
-- --------------------------------------------------------------------------
-- basics to get the data 
fn1 :: Path Abs File
fn1 =  makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/startValues/someTextWithYAML.md"
fnA = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/dataFor0163/blogA.md"

test_readmd :: IO ()
-- test with fn1 to show the pandoc 
test_readmd = do
    res1 <- runErr $ do
        mdfile <- read8 fnA markdownFileType
        pd <- readMarkdown2 mdfile
        -- putIOwords ["pd \n", showT pd, "\n--"]
        return pd
    assertEqual (Right pandocA) res1   -- set to False to produce output

-- metaY :: Meta
-- metaY = Meta {unMeta = fromList
--     [("abstract",MetaInlines [Str "The",Space,Str "long",Space,Str "struggle"]),("date",MetaInlines [Str "2020-06-16"])
--     ,("keywords",MetaInlines [Str "Haskell",Space,Str "IDE"]),("title",MetaInlines [Str "a",Space,Str "new",Space,Str "start"])
--     ]}

-- meta with bold etc...

-- metaA = Meta {unMeta = fromList [
--         ("abstract",MetaInlines [Str "An",Space,Emph [Str "abstract"],Space,Str "for",Space,Str "the",Space,Strong [Str "example"],Space,Str "A"]),
--         ("date",MetaInlines [Str "2020-06-16"]),
--         ("keywords",MetaInlines [Str "A_KEYword"]),
--         ("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title",Space,Str "of",Space,Str "A"])]}

pandocA = Pandoc  -- the contentn of blogA (fnA)
     (Meta{unMeta =
             fromList
               [("abstract",
                 MetaInlines [Str "abstract02", Space, Str "missing"]),
                ("date", MetaInlines [Str "2023-03-31"]),
                ("keywords",
                 MetaInlines [Str "one,", Space, Str "two,", Space, Str "three"]),
                ("title", MetaInlines [Str "title02", Space, Str "missing"]),
                ("version", MetaInlines [Str "publish"])]})
     [Header 1 ("02-hl1title-for-02-but-missing", [], [])
        [Str "02-hl1title", Space, Str "for", Space, Str "02", Space,
         Str "but", Space, Str "missing"],
      Para
        [Str "02-text:", Space, Str "The", Space, Str "text", Space,
         Str "for", Space, Str "02:"]]
-- pandocY :: Pandoc
-- pandocY = Pandoc (Meta {unMeta = fromList [("abstract",MetaInlines [Str "long",Space,Str "abstract"]),("date",MetaInlines [Str "2020-06-16"]),("keywords",MetaInlines [Str "KEYword"]),("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title"])]}) [Header 1 ("hl1_text-for-title",[],[]) [Str "hl1_text",Space,Emph [Str "For"],Space,Strong [Str "Title"]],Para [Str "Nonsense",Space,Str "list."],BulletList [[Plain [Str "one"]],[Plain [Str "two"]]]]
