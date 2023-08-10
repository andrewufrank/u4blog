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

defsB :: [(Text,Text)]
defsB = [("def1","def1_B"),("date","dataFalse")] 
-- simulates the insert of defaults 
-- the date nmust not be overwritten from yaml value 
test_defsB:: IO ()
test_defsB = assertEqual resB1 $ Pandoc m2 b1
    where 
            (Pandoc m1 b1) = pandocB
            m2 = addListOfDefaults defsB  m1

resB1 :: Pandoc
resB1 =  Pandoc
  (Meta{unMeta =
          fromList
            [("abstract",
              MetaBlocks
                [Para
                   [Str "This", Space, Str "blog", Space, Str "uses", Space, Str "a",
                    Space, Str "reference", Space, Str "given", Space, Str "locally.",
                    SoftBreak, Str "second", Space, Str "line"]]),
             ("bibliography", MetaInlines [Str "resources/BibTexLatex.bib"]),
             ("date", MetaInlines [Str "2010-07-29"]),
             ("def1", MetaString "def1_B"),
             ("keywords", MetaInlines [Str "referenceTest"]),
             ("reference-section-title", MetaInlines [Str "References"]),
             ("references",
              MetaList
                [MetaMap
                   (fromList
                      [("author",
                        MetaList
                          [MetaMap
                             (fromList
                                [("family", MetaInlines [Str "Fenner"]),
                                 ("given", MetaInlines [Str "Martin", Space, Str "Beat"])])]),
                       ("container-title",
                        MetaInlines [Str "Nature", Space, Str "Materials"]),
                       ("id", MetaInlines [Str "fenner2012a"]),
                       ("issued",
                        MetaMap
                          (fromList
                             [("month", MetaInlines [Str "3"]),
                              ("year", MetaInlines [Str "2212"])])),
                       ("page", MetaInlines [Str "261-263"]),
                       ("publisher",
                        MetaInlines
                          [Str "Nature", Space, Str "Publishing", Space, Str "Group"]),
                       ("title",
                        MetaInlines
                          [Str "One-click", Space, Str "science", Space, Str "marketing"]),
                       ("type", MetaInlines [Str "article-journal"]),
                       ("volume", MetaInlines [Str "11"])])]),
             ("style",
              MetaInlines
                [Str "resources/chicago-fullnote-bibliography-bb.csl"]),
             ("title",
              MetaInlines [Str "B", Space, Str "title", Space, Str "missing"]),
             ("version", MetaInlines [Str "private"])]})
  [Header 1 ("an-example-with-local-references-details", [], [])
     [Str "An", Space, Str "example", Space, Str "with", Space,
      Str "local", Space, Str "references", Space, Str "details"],
   Para
     [Str "The", Space,
      Cite
        [Citation{citationId = "frank-machbarkeit", citationPrefix = [],
                  citationSuffix = [], citationMode = NormalCitation,
                  citationNoteNum = 1, citationHash = 0}]
        [Str "[@frank-machbarkeit]"],
      Space, Str "and", Space, Str "the", Space, Str "next", Space,
      Str "in", Space, Str "a", Space, Str "foonote",
      Note
        [Para
           [Str "with", Space, Str "a", Space, Str "text",
            Cite
              [Citation{citationId = "frank09geo", citationPrefix = [],
                        citationSuffix = [], citationMode = NormalCitation,
                        citationNoteNum = 2, citationHash = 0}]
              [Str "[@frank09geo]"],
            Space, Str "before", Space, Str "and", Space, Str "after"]],
      Space, Str "are", Space, Str "in", Space, Str "the", Space,
      Str "biblio", Space, Str "file,", Space, Str "but", Space,
      Str "the", Space, Str "reference", Space,
      Cite
        [Citation{citationId = "fenner2012a", citationPrefix = [],
                  citationSuffix = [], citationMode = NormalCitation,
                  citationNoteNum = 3, citationHash = 0}]
        [Str "[@fenner2012a]"],
      Space, Str "is", Space, Str "given", Space, Str "in", Space,
      Str "the", Space, Str "file", Space, Str "locally", Space,
      Str "(note", Space, Str "the", Space, Str "format", Space,
      Str "and", Space, Str "keywords!)."]]



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

test_bodyB = do
    res1 <- runErr $ md2Meta_Process resB1
    assertEqual (Right resBWithBody1) res1

resBWithBody1 = Meta{unMeta =
          fromList
            [("abstract",
              MetaBlocks
                [Para
                   [Str "This", Space, Str "blog", Space, Str "uses", Space, Str "a",
                    Space, Str "reference", Space, Str "given", Space, Str "locally.",
                    SoftBreak, Str "second", Space, Str "line"]]),
             ("bibliography", MetaInlines [Str "resources/BibTexLatex.bib"]),
             ("body",
              MetaBlocks
                [Header 1 ("an-example-with-local-references-details", [], [])
                   [Str "An", Space, Str "example", Space, Str "with", Space,
                    Str "local", Space, Str "references", Space, Str "details"],
                 Para
                   [Str "The", Space,
                    Cite
                      [Citation{citationId = "frank-machbarkeit", citationPrefix = [],
                                citationSuffix = [], citationMode = NormalCitation,
                                citationNoteNum = 1, citationHash = 0}]
                      [Str "(Frank", Space, Str "2014)"],
                    Space, Str "and", Space, Str "the", Space, Str "next", Space,
                    Str "in", Space, Str "a", Space, Str "foonote",
                    Note
                      [Para
                         [Str "with", Space, Str "a", Space, Str "text",
                          Cite
                            [Citation{citationId = "frank09geo", citationPrefix = [],
                                      citationSuffix = [], citationMode = NormalCitation,
                                      citationNoteNum = 2, citationHash = 0}]
                            [Str "(Frank", Space, Str "2009)"],
                          Space, Str "before", Space, Str "and", Space, Str "after"]],
                    Space, Str "are", Space, Str "in", Space, Str "the", Space,
                    Str "biblio", Space, Str "file,", Space, Str "but", Space,
                    Str "the", Space, Str "reference", Space,
                    Cite
                      [Citation{citationId = "fenner2012a", citationPrefix = [],
                                citationSuffix = [], citationMode = NormalCitation,
                                citationNoteNum = 3, citationHash = 0}]
                      [Str "(Fenner", Space, Str "2212)"],
                    Space, Str "is", Space, Str "given", Space, Str "in", Space,
                    Str "the", Space, Str "file", Space, Str "locally", Space,
                    Str "(note", Space, Str "the", Space, Str "format", Space,
                    Str "and", Space, Str "keywords!)."],
                 Header 1 ("bibliography", ["unnumbered"], []) [Str "References"],
                 Div ("refs", ["references", "csl-bib-body", "hanging-indent"], [])
                   [Div ("ref-fenner2012a", ["csl-entry"], [])
                      [Para
                         [Str "Fenner,", Space, Str "Martin", Space, Str "Beat.", Space,
                          Str "2212.", Space,
                          Span ("", [], [])
                            [Str "\8220", Str "One-Click", Space, Str "Science", Space,
                             Str "Marketing", Str ".", Str "\8221"],
                          Space, Emph [Str "Nature", Space, Str "Materials"], Space,
                          Str "11", Space, Str "(March):", Space, Str "261\8211\&63."]],
                    Div ("ref-frank09geo", ["csl-entry"], [])
                      [Para
                         [Str "Frank,", Space, Str "Andrew", Space, Str "U.", Space,
                          Str "2009.", Space,
                          Span ("", [], [])
                            [Str "\8220", Str "Geo-Ontologies", Space, Str "Are", Space,
                             Str "Scale", Space, Str "Dependent", Space, Str "(Abstract", Space,
                             Str "Only)", Str ".", Str "\8221"],
                          Space, Str "In", Space,
                          Emph
                            [Str "European", Space, Str "Geosciences", Space, Str "Union,",
                             Space, Str "General", Space, Str "Assembly", Space, Str "2009,",
                             Space, Str "Session", Space, Str "Knowledge", Space, Str "and",
                             Space, Str "Ontologies"],
                          Str ",", Space, Str "edited", Space, Str "by", Space, Str "Tuija",
                          Space, Str "Pulkkinen.", Space,
                          Link ("", [], [])
                            [Str "http://publik.tuwien.ac.at/files/PubDat-175453.pdf"]
                            ("http://publik.tuwien.ac.at/files/PubDat-175453.pdf", ""),
                          Str "."]],
                    Div ("ref-frank-machbarkeit", ["csl-entry"], [])
                      [Para
                         [Str "\8212\8212\8212.", Space, Str "2014.", Space,
                          Span ("", [], [])
                            [Str "\8220", Str "Machbarkeit", Space, Str "eines", Space,
                             Str "InformaInformations", Space, Str "f\252r", Space,
                             Str "geographische", Space, Str "Daten", Str ".", Str "\8221"],
                          Space, Str "Geoinformation,", Space, Str "Technische", Space,
                          Str "Universitaet", Space, Str "Wien."]]]]),
             ("date", MetaInlines [Str "2010-07-29"]),
             ("def1", MetaString "def1_B"),
             ("keywords", MetaInlines [Str "referenceTest"]),
             ("reference-section-title", MetaInlines [Str "References"]),
             ("references",
              MetaList
                [MetaMap
                   (fromList
                      [("author",
                        MetaList
                          [MetaMap
                             (fromList
                                [("family", MetaInlines [Str "Fenner"]),
                                 ("given", MetaInlines [Str "Martin", Space, Str "Beat"])])]),
                       ("container-title",
                        MetaInlines [Str "Nature", Space, Str "Materials"]),
                       ("id", MetaInlines [Str "fenner2012a"]),
                       ("issued",
                        MetaMap
                          (fromList
                             [("month", MetaInlines [Str "3"]),
                              ("year", MetaInlines [Str "2212"])])),
                       ("page", MetaInlines [Str "261-263"]),
                       ("publisher",
                        MetaInlines
                          [Str "Nature", Space, Str "Publishing", Space, Str "Group"]),
                       ("title",
                        MetaInlines
                          [Str "One-click", Space, Str "science", Space, Str "marketing"]),
                       ("type", MetaInlines [Str "article-journal"]),
                       ("volume", MetaInlines [Str "11"])])]),
             ("style",
              MetaInlines
                [Str "resources/chicago-fullnote-bibliography-bb.csl"]),
             ("title",
              MetaInlines [Str "B", Space, Str "title", Space, Str "missing"]),
             ("version", MetaInlines [Str "private"])]}
-- --------------------------------------------------------------------------
-- basics to get the data 
fn1 :: Path Abs File
fn1 =  makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/startValues/someTextWithYAML.md"
fnA = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/dataFor0163/blogA.md"
fnB = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/data/dataFor0163/blogB.md"

test_readmd :: IO ()
-- test with fn1 to show the pandoc 
test_readmd = do
    res1 <- runErr $ do
        mdfile <- read8 fnA markdownFileType
        pd <- readMarkdown2 mdfile
        -- putIOwords ["pd \n", showT pd, "\n--"]
        return pd
    assertEqual (Right pandocA) res1   -- set to False to produce output

test_readmdB :: IO ()
-- test with fn1 to show the pandoc 
test_readmdB = do
    res1 <- runErr $ do
        mdfile <- read8 fnB markdownFileType
        pd <- readMarkdown2 mdfile
        -- putIOwords ["pd \n", showT pd, "\n--"]
        return pd
    assertEqual (Right pandocB) res1   -- set to False to produce output



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

pandocB = Pandoc
     (Meta{unMeta =
             fromList
               [("abstract",
                 MetaBlocks
                   [Para
                      [Str "This", Space, Str "blog", Space, Str "uses", Space, Str "a",
                       Space, Str "reference", Space, Str "given", Space, Str "locally.",
                       SoftBreak, Str "second", Space, Str "line"]]),
                ("bibliography", MetaInlines [Str "resources/BibTexLatex.bib"]),
                ("date", MetaInlines [Str "2010-07-29"]),
                ("keywords", MetaInlines [Str "referenceTest"]),
                ("reference-section-title", MetaInlines [Str "References"]),
                ("references",
                 MetaList
                   [MetaMap
                      (fromList
                         [("author",
                           MetaList
                             [MetaMap
                                (fromList
                                   [("family", MetaInlines [Str "Fenner"]),
                                    ("given",
                                     MetaInlines [Str "Martin", Space, Str "Beat"])])]),
                          ("container-title",
                           MetaInlines [Str "Nature", Space, Str "Materials"]),
                          ("id", MetaInlines [Str "fenner2012a"]),
                          ("issued",
                           MetaMap
                             (fromList
                                [("month", MetaInlines [Str "3"]),
                                 ("year", MetaInlines [Str "2212"])])),
                          ("page", MetaInlines [Str "261-263"]),
                          ("publisher",
                           MetaInlines
                             [Str "Nature", Space, Str "Publishing", Space, Str "Group"]),
                          ("title",
                           MetaInlines
                             [Str "One-click", Space, Str "science", Space, Str "marketing"]),
                          ("type", MetaInlines [Str "article-journal"]),
                          ("volume", MetaInlines [Str "11"])])]),
                ("style",
                 MetaInlines
                   [Str "resources/chicago-fullnote-bibliography-bb.csl"]),
                ("title",
                 MetaInlines [Str "B", Space, Str "title", Space, Str "missing"]),
                ("version", MetaInlines [Str "private"])]})
     [Header 1 ("an-example-with-local-references-details", [], [])
        [Str "An", Space, Str "example", Space, Str "with", Space,
         Str "local", Space, Str "references", Space, Str "details"],
      Para
        [Str "The", Space,
         Cite
           [Citation{citationId = "frank-machbarkeit", citationPrefix = [],
                     citationSuffix = [], citationMode = NormalCitation,
                     citationNoteNum = 1, citationHash = 0}]
           [Str "[@frank-machbarkeit]"],
         Space, Str "and", Space, Str "the", Space, Str "next", Space,
         Str "in", Space, Str "a", Space, Str "foonote",
         Note
           [Para
              [Str "with", Space, Str "a", Space, Str "text",
               Cite
                 [Citation{citationId = "frank09geo", citationPrefix = [],
                           citationSuffix = [], citationMode = NormalCitation,
                           citationNoteNum = 2, citationHash = 0}]
                 [Str "[@frank09geo]"],
               Space, Str "before", Space, Str "and", Space, Str "after"]],
         Space, Str "are", Space, Str "in", Space, Str "the", Space,
         Str "biblio", Space, Str "file,", Space, Str "but", Space,
         Str "the", Space, Str "reference", Space,
         Cite
           [Citation{citationId = "fenner2012a", citationPrefix = [],
                     citationSuffix = [], citationMode = NormalCitation,
                     citationNoteNum = 3, citationHash = 0}]
           [Str "[@fenner2012a]"],
         Space, Str "is", Space, Str "given", Space, Str "in", Space,
         Str "the", Space, Str "file", Space, Str "locally", Space,
         Str "(note", Space, Str "the", Space, Str "format", Space,
         Str "and", Space, Str "keywords!)."]]

-- pandocY :: Pandoc
-- pandocY = Pandoc (Meta {unMeta = fromList [("abstract",MetaInlines [Str "long",Space,Str "abstract"]),("date",MetaInlines [Str "2020-06-16"]),("keywords",MetaInlines [Str "KEYword"]),("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title"])]}) [Header 1 ("hl1_text-for-title",[],[]) [Str "hl1_text",Space,Emph [Str "For"],Space,Strong [Str "Title"]],Para [Str "Nonsense",Space,Str "list."],BulletList [[Plain [Str "one"]],[Plain [Str "two"]]]]
