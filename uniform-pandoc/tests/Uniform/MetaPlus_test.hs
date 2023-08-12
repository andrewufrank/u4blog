---------------------------------------------------------------------------
--
-- Module      :  metaplus  record construction
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
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.MetaPlus_test where

import Test.Framework
-- import qualified Data.Map as M 
---- using uniform:
import Uniform.Json (ToJSON, FromJSON, toJSON)
import Uniform.PandocImports ( Meta(..), Pandoc(..) ) 
import Uniform.Markdown ( markdownFileType, readMarkdown2 ) 
import Uniform.TexWriter 
import Uniform.PandocHTMLwriter  
import Text.Pandoc as Pandoc 
    
import Text.Pandoc.Definition ()
import Text.Pandoc.Writers ()
import Text.DocTemplates as DocTemplates ()
import Text.DocLayout (render, Doc)
import Data.Text.Lazy (unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Map as M
import Data.Map ( fromList, toList) 
-- import Uniform.Test.TestHarness ()
import Uniform.MetaStuff
import Uniform.MetaPlus
import Uniform.MetaStuff_test
import Uniform.TemplateStuff
import Uniform.TemplateStuff_test
import Uniform.HttpFiles
import UniformBase
import Uniform.TexFileTypes (texFileType, Latex(Latex))

-- tests for 0.1.6.3



extra1 = ExtraValues {extraDainoVersion = "0.1.5.6.3"
                    , extraBakedDir = "/home/frank/baked"}

metap1 = MetaPlus { metap = resAWithBody1
                   , sett = settings1
                   , extra = extra1
                   , metaMarkdown = resBody
                   , metaHtml = resBodyhtml
                   , metaLatex = zero 
                   } 


-- test_mp1 = assertEqual (zero) metap1

-- read settings file -- simpler use a set (and simplified data set)

-- settingsfn = makeAbsFile "/home/frank/Workspace11/daino/settings3.yaml"

-- test_settings = do 
--     res1 <- runErr $ do 
--         readSettings NoticeLevel2 settingsfn 
--     assertEqual (Right zero) res1

test_lookup :: IO ()
-- check that lookup gives default values 
test_lookup = assertEqual "defAuth" $ getTextFromYaml6 "defAuth" "author" (metap metap2)

-- produce a markdown body 
test_markdownBody = do 
    res1 <- runErr $ do 
        meta2xx writeToMarkdown  resAWithBody1 
    -- putIOwords ["test_markdownBody resAWithBody1", showT resAWithBody1]
    -- putIOwords ["test_markdownBody res1 from met2xx", showT res1]
    assertEqual (Right resBody) res1 
resBody = fromList
     [("abstract", "abstract02 missing\n"),
      ("body",
       "# 02-hl1title for 02 but missing\n\n02-text: The text for 02:\n"),
      ("date", "2023-03-31\n"), ("def1", "def1v\n"),
      ("keywords", "one, two, three\n"), ("title", "title02 missing\n"),
      ("version", "publish\n")]

-- produce a html body 
test_htmlBody = do 
    res1 <- runErr $ do 
        meta2xx writeHtml5String2  resAWithBody1 
    -- putIOwords ["test_htmlBodyresAWithBody1", showT resAWithBody1]
    -- putIOwords ["test_htmlBodyres1 from met2xx", showT res1]
    assertEqual (Right resBodyhtml) res1 
resBodyhtml = fromList
     [("abstract", "abstract02 missing"),
      ("body",
       "<h1 id=\"02-hl1title-for-02-but-missing\">02-hl1title for 02 but\nmissing</h1>\n<p>02-text: The text for 02:</p>"),
      ("date", "2023-03-31"), ("def1", "def1v"),
      ("keywords", "one, two, three"), ("title", "title02 missing"),
      ("version", "publish")]

metap2 = MetaPlus { metap = resAWithBody1
                   , sett = settings1
                   , extra = extra1
                   , metaMarkdown = zero
                   , metaHtml = zero
                   , metaLatex = zero
                   } 

-- fill the three meta fields for the output
completeMetaPlus :: MetaPlus -> ErrIO MetaPlus 
completeMetaPlus metapl1 = do 
    md1 <- meta2xx writeToMarkdown  (metap metapl1)
    htm1 <- meta2xx writeHtml5String2 (metap metapl1)
    lat1 <- meta2xx writeTexSnip2 (metap metapl1)
    -- uses biblatex
    let metap2 = metapl1  { metaMarkdown = md1
                    , metaHtml = htm1
                    , metaLatex = lat1}
    -- putIOwords ["completeMetaPlus \n", showT metap2]
    return metap2 
     

metaplusText = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/resources/metaplusText.dtpl"   
metaplusHtml = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/resources/metaplusHtml.dtpl"   
metaplusLatex = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/resources/metaplusLatex.dtpl"   
fnPlusres =  makeAbsFile "/home/frank/tests/testMetaPlus"

test_templ_comp_miniplus :: IO ()
test_templ_comp_miniplus = do 
    res1 <- runErr $ do 
        metap3 <- completeMetaPlus metap1 
        ttpl2 <- compileTemplateFile2 metaplusText -- fnminilatex
        let tpl1 = renderTemplate ttpl2 (toJSON metap3)  :: Doc Text
        -- putIOwords ["tpl1 \n", showT tpl1]
        let text1 = render (Just 50) tpl1  -- line length, can be Nothing
        -- putIOwords ["test_templ_comp_miniplus res1 \n", text1]

        htpl2 <- compileTemplateFile2 metaplusHtml -- fnminilatex
        let hpl1 = renderTemplate htpl2 (toJSON metap3)  :: Doc Text
        -- putIOwords ["test_templ_comp_miniplus tpl1 \n", showT tpl1]
        let ht1 = render (Just 50) hpl1  -- line length, can be Nothing
        -- putIOwords ["test_templ_comp_miniplus res1 \n", res1]
        write8   fnPlusres htmloutFileType (HTMLout ht1)

        ltpl2 <- compileTemplateFile2 metaplusLatex -- fnminilatex
        let lpl1 = renderTemplate ltpl2 (toJSON metap3)  :: Doc Text
        -- putIOwords ["test_templ_comp_miniplus tpl1 \n", showT tpl1]
        let latex1 = render (Just 50) lpl1  -- line length, can be Nothing
        -- putIOwords ["test_templ_comp_miniplus res1 \n", res1]
        write8   fnPlusres texFileType (Latex latex1)

        return text1
    assertEqual (Right resPlusRes) res1

resPlusRes = 
    "\n    \n-- from YAML header from Markdown\n    title: title02 missing\n    abstract: abstract02 missing\n    keywords: one, two, three \n    version: publish\n    date: 2023-03-31 \n    body:  # 02-hl1title for 02 but missing\n\n02-text: The text for 02:  \n\n-- from YAML header from html \n    title: title02 missing\n    abstract: abstract02 missing\n    keywords: one, two, three \n    version: publish\n    date: 2023-03-31 \n    body:  <h1 id=\"02-hl1title-for-02-but-missing\">02-hl1title for 02 but\nmissing</h1>\n<p>02-text: The text for 02:</p>  \n\n-- from YAML header from latex\n    title: title02 missing\n    abstract: abstract02 missing\n    keywords: one, two, three \n    version: publish\n    date: 2023-03-31 \n    body:  \\hypertarget{02-hl1title-for-02-but-missing}{%\n\\section{02-hl1title for 02 but\nmissing}\\label{02-hl1title-for-02-but-missing}}\n\n02-text: The text for 02:  -- from Defaults   \n   def1: def1v  \n\n-- from Defaults   \n   def1: def1v  \n\n-- from extra \n   dainoVersion:    \n   bakedDir:   \n\n-- from settings\n    siteHeader: \n    menu: \n                    link: \n            text: \n        "

--------------------blogB  test with references

metapB = MetaPlus { metap = resBWithBody1
                   , sett = settings1
                   , extra = extra1
                   , metaMarkdown = zero
                   , metaHtml = zero
                   , metaLatex = zero
                   } 



metaplusText2 = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/resources/metaplusText2.dtpl"   
fnPlusB =  makeAbsFile "/home/frank/tests/testminiMetaPlusB"

test_templ_comp_miniplusB :: IO ()
test_templ_comp_miniplusB = do 
    res1 <- runErr $ do 
        metap3 <- completeMetaPlus metapB
        ttpl2 <- compileTemplateFile2 metaplusText -- fnminilatex
        let tpl1 = renderTemplate ttpl2 (toJSON metap3)  :: Doc Text
        -- putIOwords ["tpl1 \n", showT tpl1]
        let text1 = render (Just 50) tpl1  -- line length, can be Nothing
        -- putIOwords ["res1 \n", text1]

        htpl2 <- compileTemplateFile2 metaplusHtml -- fnminilatex
        let hpl1 = renderTemplate htpl2 (toJSON metap3)  :: Doc Text
        -- putIOwords ["tpl1 \n", showT tpl1]
        let ht1 = render (Just 50) hpl1  -- line length, can be Nothing
        -- putIOwords ["res1 \n", res1]
        write8   fnPlusB htmloutFileType (HTMLout ht1)

        ltpl2 <- compileTemplateFile2 metaplusLatex -- fnminilatex
        let lpl1 = renderTemplate ltpl2 (toJSON metap3)  :: Doc Text
        -- putIOwords ["tpl1 \n", showT tpl1]
        let latex1 = render (Just 50) lpl1  -- line length, can be Nothing
        -- putIOwords ["res1 \n", res1]
        write8   fnPlusB texFileType (Latex latex1)

        return text1
    assertEqual (Right resPlusB) res1

resPlusB = 
   "\n    \n-- from YAML header from Markdown\n    title: B title missing\n    abstract: This blog uses a reference given locally. second line\n    keywords: referenceTest \n    version: private\n    date: 2010-07-29 \n    body:  # An example with local references details\n\nThe (Frank 2014) and the next in a foonote[1] are in the biblio file,\nbut the reference (**fenner2012a?**) is given in the file locally (note\nthe format and keywords!).\n\n# References\n\nFrank, Andrew U. 2009. \8220Geo-Ontologies Are Scale Dependent (Abstract\nOnly).\8221 In *European Geosciences Union, General Assembly 2009, Session\nKnowledge and Ontologies*, edited by Tuija Pulkkinen.\n<http://publik.tuwien.ac.at/files/PubDat-175453.pdf>.\n\n\8212\8212\8212. 2014. \8220Machbarkeit eines InformaInformations f\252r geographische\nDaten.\8221 Geoinformation, Technische Universitaet Wien.\n\n[1] with a text(Frank 2009) before and after  \n\n-- from YAML header from html \n    title: B title missing\n    abstract: <p>This blog uses a reference given locally. second line</p>\n    keywords: referenceTest \n    version: private\n    date: 2010-07-29 \n    body:  <h1 id=\"an-example-with-local-references-details\">An example with local\nreferences details</h1>\n<p>The <span class=\"citation\" data-cites=\"frank-machbarkeit\">(Frank\n2014)</span> and the next in a foonote<a href=\"#fn1\"\nclass=\"footnote-ref\" id=\"fnref1\" role=\"doc-noteref\"><sup>1</sup></a> are\nin the biblio file, but the reference <span class=\"citation\"\ndata-cites=\"fenner2012a\">(<strong>fenner2012a?</strong>)</span> is given\nin the file locally (note the format and keywords!).</p>\n<h1 class=\"unnumbered\" id=\"bibliography\">References</h1>\n<div id=\"refs\" class=\"references csl-bib-body hanging-indent\"\nrole=\"list\">\n<div id=\"ref-frank09geo\" class=\"csl-entry\" role=\"listitem\">\nFrank, Andrew U. 2009. <span>\8220Geo-Ontologies Are Scale Dependent\n(Abstract Only).\8221</span> In <em>European Geosciences Union, General\nAssembly 2009, Session Knowledge and Ontologies</em>, edited by Tuija\nPulkkinen. <a\nhref=\"http://publik.tuwien.ac.at/files/PubDat-175453.pdf\">http://publik.tuwien.ac.at/files/PubDat-175453.pdf</a>.\n</div>\n<div id=\"ref-frank-machbarkeit\" class=\"csl-entry\" role=\"listitem\">\n\8212\8212\8212. 2014. <span>\8220Machbarkeit eines InformaInformations f\252r\ngeographische Daten.\8221</span> Geoinformation, Technische Universitaet\nWien.\n</div>\n</div>\n<aside id=\"footnotes\" class=\"footnotes footnotes-end-of-document\"\nrole=\"doc-endnotes\">\n<hr />\n<ol>\n<li id=\"fn1\"><p>with a text<span class=\"citation\"\ndata-cites=\"frank09geo\">(Frank 2009)</span> before and after<a\nhref=\"#fnref1\" class=\"footnote-back\" role=\"doc-backlink\">\8617\65038</a></p></li>\n</ol>\n</aside>  \n\n-- from YAML header from latex\n    title: B title missing\n    abstract: This blog uses a reference given locally. second line\n    keywords: referenceTest \n    version: private\n    date: 2010-07-29 \n    body:  \\hypertarget{an-example-with-local-references-details}{%\n\\section{An example with local references\ndetails}\\label{an-example-with-local-references-details}}\n\nThe \\autocite{frank-machbarkeit} and the next in a foonote\\footnote{with\n  a text\\autocite{frank09geo} before and after} are in the biblio file,\nbut the reference \\autocite{fenner2012a} is given in the file locally\n(note the format and keywords!).  -- from Defaults   \n   def1: def1\\_B  \n\n-- from Defaults   \n   def1: def1\\_B  \n\n-- from extra \n   dainoVersion:    \n   bakedDir:   \n\n-- from settings\n    siteHeader: \n    menu: \n                    link: \n            text: \n        "




settings1 :: Settings
settings1 = zero :: Settings 
    -- Settings {
    --     -- siteLayout = SiteLayout {themeDir = Path Abs Dir /home/frank/Workspace11/daino/docs/theme/, doughDir = Path Abs Dir /home/frank/Workspace11/daino/docs/site/dough/, bakedDir = Path Abs Dir /home/frank/Workspace11/daino/docs/site/baked/, masterTemplateFile = Path Rel File master5.dtpl}, 
    --     localhostPort = 3000, settingsAuthor = "Author of Settings", settingsDate = "2019-01-01", siteHeader = SiteHeader {sitename = "siteNameExample", byline = "siteByLineExample", banner = "/templates/img/symmetricGeras2.jpg"}, menu = [MenuItem {navlink = "/Blog/index.html", navtext = "Blog"},MenuItem {navlink = "/PublicationList/index.html", navtext = "Publications"},MenuItem {navlink = "/dainodesign/index.html", navtext = "daino Documentation"}]}

