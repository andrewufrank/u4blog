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
import Uniform.TexWriter ()
import Uniform.PandocHTMLwriter  
import Text.Pandoc
    
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
import Uniform.TemplateStuff
import Uniform.TemplateStuff_test
import Uniform.HttpFiles

import UniformBase

-- tests for 0.1.6.3

data MetaPlus = MetaPlus 
                { metap :: Meta    -- ^ the pandoc meta 
                , extra :: ExtraValues -- ^ other values to go into template
                , mapHtml ::  M.Map Text Text}
    deriving (Eq, Ord, Show, Read, Generic) -- Zeros, ToJSON, FromJSON)
instance ToJSON MetaPlus
instance FromJSON MetaPlus
instance Zeros MetaPlus where zero :: MetaPlus
                              zero = MetaPlus zero zero zero

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

metap1 = MetaPlus { metap = resAWithBody
                   , extra = extra1
                   , mapHtml = resAhtml} 
                   
-- test_mp1 = assertEqual (zero) metap1                   

fnminiPlusres =  makeAbsFile "/home/frank/tests/testminiMetaPlus"

test_templ_comp_miniplus :: IO ()
test_templ_comp_miniplus = do 
    res1 <- runErr $ do 
        htpl2 <- compileTemplateFile2 fnminihtml -- fnminilatex
        let tpl1 = renderTemplate htpl2 (toJSON metap1)  :: Doc Text
        -- putIOwords ["tpl1 \n", showT tpl1]
        let res1 = render (Just 50) tpl1  -- line length, can be Nothing

        -- putIOwords ["res1 \n", showT res1]
        write8   fnminiPlusres htmloutFileType (HTMLout res1)
        return res1
    assertEqual (Right resPlusRes) res1

resPlusRes = "\n    \n    <!doctype html>\n    <html>\n    <head>\n    <title>title02 missing</title><br>\n    <meta name=\"description\" content=abstract02 missing><br>\n    <meta name=\"keywords\" content=one, two, three><br>\n    </head>\n    <body>\n   title: title02 missing <br>\n   version:  <br>\n   date: 2023-03-31 <br>\n   def1:  <br>\n   dainoVersion: 0.1.5.6.3  <br>\n   bakedDir: /home/frank/baked <br>\n   body: <br> <h1 id=\"02-hl1title-for-02-but-missing\">02-hl1title for 02 but\nmissing</h1>\n<p>02-text: The text for 02:</p> <br>\n    </body>\n    </html>\n"
                   
--- tests from 0.1.6.2----------------

-- -- test getFromYaml value on abstract
-- test_ka = assertEqual (Just abs1) $ getFromYaml "abstract" pandocY
-- -- test getFromYaml with default as metavlaue, but value is present
-- test_fa = assertEqual abs1 $
--              getMetaValueFromYaml4 "oneAbstract" "abstract" pandocY
-- -- get as Text 
-- test_faT = assertEqual "long abstract" $
--              getTextFromYaml5 "oneAbstract" "abstract" pandocY


-- abs2 :: MetaValue
-- abs2r :: [Block]
-- abs2r = [Plain
--      [Str "An", Space, Emph [Str "abstract"], Space, Str "for", Space,
--       Str "the", Space, Strong [Str "example"], Space, Str "A"]]

-- abs2 = MetaInlines [Str "An",Space,Emph [Str "abstract"],Space,Str "for",Space,Str "the",Space,Strong [Str "example"],Space,Str "A"]
-- test_mvb = assertEqual (Just abs2r) $ metaValueToBlock abs2

-- abs1:: MetaValue
-- abs1 =  MetaInlines [Str "long", Space, Str "abstract"]


-- -- check conversion of metavalue to htmltext 
-- test_block_html :: IO ()
-- test_block_html = do 
--     res1 <- runErr $ do 
--         block2xx writeHtml5String2 abs2r
--     assertEqual (Right abs2html) $ res1
-- abs2html = "An <em>abstract</em> for the <strong>example</strong> A"

-- -- check conversion of metavalue to text 
-- test_mvt :: IO ()
-- test_mvt = assertEqual abs1t $ metaValueToText abs1

-- test_metaval_html :: IO ()
-- test_metaval_html = do 
--     res1 <- runErr $ do 
--         metaValue2xx writeHtml5String2 abs2
--     assertEqual (Right abs2html) $ res1

    
-- abs1t :: Maybe Text
-- abs1t = Just "long abstract"
-- -- ad a value to meta
-- key1 :: Text
-- key1 = "indexEntry" :: Text 
-- test_addMeta :: IO ()
-- test_addMeta = assertEqual testval1 $
--             getTextFromYaml5 "def" key1 .
--             meta2pandoc .    addMetaFieldT key1 (testval1) $ metaY 

-- -- check that the value is stored and can be retrieved
-- testval1 :: Text
-- testval1 = "A test Text value" :: Text 
-- test_addPandoc :: IO ()
-- test_addPandoc = assertEqual (Just . MetaString $ testval1) $ 
--             getFromYaml key1 . addMetaField2pandoc key1 testval1 $ pandocY


-- testval2 :: Text
-- testval2= "A test Text value" :: Text 
-- test_setgetmeta :: IO ()
-- test_setgetmeta = assertEqual (testval2) $ 
--             flip getValue4meta key1 . setValue2meta key1 testval2 $ metaY



-- test_map2list :: IO ()
-- test_map2list = assertEqual [("body", MetaString "xx")] $ toList (unMeta meta1)
-- test_map2listadd :: IO ()
-- test_map2listadd = assertEqual cont12 $ toList . unMeta $ Meta (fromList [(("abst"::Text), (MetaString "yy"))]) <>  meta1

-- meta1 :: Meta
-- meta1 = Meta (fromList [(("body"::Text), (MetaString "xx"))])
-- cont12 :: [(Text, MetaValue)]
-- cont12 =  [("abst", MetaString "yy"), ("body", MetaString "xx")]

defs1 :: [(Text,Text)]
defs1 = [("def1","def1v"),("date","dataFalse")] -- the date nmust not be overwritten from yaml value 
test_defs :: IO ()
test_defs = assertEqual defs1res $ addListOfDefaults defs1 (getMeta pandocA)

defs1res :: Meta
defs1res = Meta{unMeta =
       fromList
         [("abstract",
           MetaInlines [Str "abstract02", Space, Str "missing"]),
          ("date", MetaInlines [Str "2023-03-31"]),
          ("def1", MetaString "def1v"),
          ("keywords",
           MetaInlines [Str "one,", Space, Str "two,", Space, Str "three"]),
          ("title", MetaInlines [Str "title02", Space, Str "missing"]),
          ("version", MetaInlines [Str "publish"])]}

test_usemeta :: IO ()
-- test with fn1 to show the pandoc 
test_usemeta = do 
    res1 <- runErr $ do 
        m1 <- meta2xx  writeHtml5String2 resAWithBody
        return m1
    assertEqual (Right resAhtml) res1   -- set to False to produce output
resAhtml :: M.Map Text Text 
resAhtml = fromList [("abstract", "abstract02 missing"),
      ("body",
       "<h1 id=\"02-hl1title-for-02-but-missing\">02-hl1title for 02 but\nmissing</h1>\n<p>02-text: The text for 02:</p>"),
      ("date", "2023-03-31"), ("keywords", "one, two, three"),
      ("title", "title02 missing"), ("version", "publish")]

-- process cites and put body into meta 
test_body = do 
    res1 <- runErr $ md2Meta_Process pandocA 
    assertEqual (Right resAWithBody) res1 

resAWithBody :: Meta
resAWithBody = Meta{unMeta =
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
             ("keywords",
              MetaInlines [Str "one,", Space, Str "two,", Space, Str "three"]),
             ("title", MetaInlines [Str "title02", Space, Str "missing"]),
             ("version", MetaInlines [Str "publish"])]}
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

metaY :: Meta 
metaY = Meta {unMeta = fromList 
    [("abstract",MetaInlines [Str "The",Space,Str "long",Space,Str "struggle"]),("date",MetaInlines [Str "2020-06-16"])
    ,("keywords",MetaInlines [Str "Haskell",Space,Str "IDE"]),("title",MetaInlines [Str "a",Space,Str "new",Space,Str "start"])
    ]}

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
pandocY :: Pandoc 
pandocY = Pandoc (Meta {unMeta = fromList [("abstract",MetaInlines [Str "long",Space,Str "abstract"]),("date",MetaInlines [Str "2020-06-16"]),("keywords",MetaInlines [Str "KEYword"]),("title",MetaInlines [Str "the",Space,Strong [Str "real"],Space,Str "title"])]}) [Header 1 ("hl1_text-for-title",[],[]) [Str "hl1_text",Space,Emph [Str "For"],Space,Strong [Str "Title"]],Para [Str "Nonsense",Space,Str "list."],BulletList [[Plain [Str "one"]],[Plain [Str "two"]]]] 
 