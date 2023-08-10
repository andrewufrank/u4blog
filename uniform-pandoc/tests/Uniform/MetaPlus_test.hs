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
import Uniform.Test.TestHarness ()
import Uniform.MetaStuff
import Uniform.MetaStuff_test
import Uniform.TemplateStuff
import Uniform.TemplateStuff_test
import Uniform.HttpFiles
import UniformBase

-- tests for 0.1.6.3

data MetaPlus = MetaPlus 
                { metap :: Meta    -- ^ the pandoc meta 
                , sett :: Settings -- ^ the data from the settingsfile
                , extra :: ExtraValues -- ^ other values to go into template
                , metaMarkdown :: M.Map Text Text 
                , metaHtml ::  M.Map Text Text
                , metaLatex ::  M.Map Text Text
                }
    deriving (Eq, Ord, Show, Read, Generic) -- Zeros, ToJSON, FromJSON)
instance ToJSON MetaPlus
instance FromJSON MetaPlus
instance Zeros MetaPlus where zero :: MetaPlus
                              zero = MetaPlus zero zero zero zero zero zero

instance Zeros (M.Map Text Text) where zero = fromList []

data ExtraValues = ExtraValues 
                        { dainoVersion:: Text
                        , bakedDir :: Text
                        }
    deriving (Eq, Ord, Show, Read, Generic)
    
instance ToJSON ExtraValues 
instance FromJSON ExtraValues 

instance Zeros ExtraValues where zero :: ExtraValues
                                 zero = ExtraValues zero zero 

extra1 = ExtraValues {dainoVersion = "0.1.5.6.3"
                    , bakedDir = "/home/frank/baked"}

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


-- produce a markdown body 
test_markdownBody = do 
    res1 <- runErr $ do 
        meta2xx writeToMarkdown  resAWithBody1 
    putIOwords ["resAWithBody1", showT resAWithBody1]
    putIOwords ["res1 from met2xx", showT res1]
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
    putIOwords ["resAWithBody1", showT resAWithBody1]
    putIOwords ["res1 from met2xx", showT res1]
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
    let metap2 = metapl1  { metaMarkdown = md1
                    , metaHtml = htm1
                    , metaLatex = lat1}
    putIOwords ["completeMetaPlus \n", showT metap2]
    return metap2 
     

metaplusText = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-pandoc/resources/metaplusText.dtpl"   
fnminiPlusres =  makeAbsFile "/home/frank/tests/testminiMetaPlus"

test_templ_comp_miniplus :: IO ()
test_templ_comp_miniplus = do 
    res1 <- runErr $ do 
        metap3 <- completeMetaPlus metap1 
        htpl2 <- compileTemplateFile2 metaplusText -- fnminilatex
        let tpl1 = renderTemplate htpl2 (toJSON metap3)  :: Doc Text
        -- putIOwords ["tpl1 \n", showT tpl1]
        let res1 = render (Just 50) tpl1  -- line length, can be Nothing

        putIOwords ["res1 \n", res1]
        write8   fnminiPlusres htmloutFileType (HTMLout res1)
        return res1
    assertEqual (Right resPlusRes) res1

resPlusRes = "\n    \n-- from YAML header from Markdown\n    title: title02 missing\n    abstract: abstract02 missing\n    keywords: one, two, three \n    version: publish\n    date: 2023-03-31 \n    body:  # 02-hl1title for 02 but missing\n\n02-text: The text for 02:  \n\n-- from YAML header from html \n    title: title02 missing\n    abstract: abstract02 missing\n    keywords: one, two, three \n    version: publish\n    date: 2023-03-31 \n    body:  <h1 id=\"02-hl1title-for-02-but-missing\">02-hl1title for 02 but\nmissing</h1>\n<p>02-text: The text for 02:</p>  \n\n-- from YAML header from latex\n    title: title02 missing\n    abstract: abstract02 missing\n    keywords: one, two, three \n    version: publish\n    date: 2023-03-31 \n    body:  \\hypertarget{02-hl1title-for-02-but-missing}{%\n\\section{02-hl1title for 02 but\nmissing}\\label{02-hl1title-for-02-but-missing}}\n\n02-text: The text for 02:  -- from Defaults   \n   def1: def1v  \n\n-- from Defaults   \n   def1: def1v  \n\n-- from extra \n   dainoVersion: 0.1.5.6.3   \n   bakedDir: /home/frank/baked  \n\n-- from settings\n    siteHeader: siteNameExample\n    menu: \n                    link: /Blog/index.html\n            text: Blog\n                    link: /PublicationList/index.html\n            text: Publications\n                    link: /dainodesign/index.html\n            text: daino Documentation\n        "

------------ settings (copied to avoid circular import)

data Settings = Settings
    { ---  siteLayout ::  
      localhostPort :: Int 
    , settingsAuthor :: Text 
    , settingsDate :: Text -- should be UTC 
    , siteHeader :: SiteHeader 
    , menu :: [MenuItem]
    -- , today :: Text
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)

instance ToJSON Settings
instance FromJSON Settings

data SiteHeader = SiteHeader 
    { sitename :: FilePath 
    , byline :: Text 
    , banner :: FilePath 
    -- , bannerCaption :: Text 
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON SiteHeader
instance FromJSON SiteHeader

data MenuItem = MenuItem  
    { navlink :: FilePath 
    , navtext :: Text
    -- , navpdf :: Text  -- for the link to the pdf 
    -- not a good idead to put here
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON MenuItem
instance FromJSON MenuItem

settings1 :: Settings
settings1 = -- zero :: Settings 
    Settings {
        -- siteLayout = SiteLayout {themeDir = Path Abs Dir /home/frank/Workspace11/daino/docs/theme/, doughDir = Path Abs Dir /home/frank/Workspace11/daino/docs/site/dough/, bakedDir = Path Abs Dir /home/frank/Workspace11/daino/docs/site/baked/, masterTemplateFile = Path Rel File master5.dtpl}, 
        localhostPort = 3000, settingsAuthor = "Author of Settings", settingsDate = "2019-01-01", siteHeader = SiteHeader {sitename = "siteNameExample", byline = "siteByLineExample", banner = "/templates/img/symmetricGeras2.jpg"}, menu = [MenuItem {navlink = "/Blog/index.html", navtext = "Blog"},MenuItem {navlink = "/PublicationList/index.html", navtext = "Publications"},MenuItem {navlink = "/dainodesign/index.html", navtext = "daino Documentation"}]}

