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
import qualified Data.Map as M 
---- using uniform:
import Uniform.Json 
import Uniform.Pandoc 
-- import Uniform.Filenames 
-- import Uniform2.Filetypes4sites should not be imported here

import Uniform.Test.TestHarness
-- import Uniform.Markdown_test 
-- import Uniform.Error           hiding (  (<.>)  )  -- (</>)
import UniformBase
import Text.DocLayout (render)
import Text.DocTemplates as DocTemplates

-- works only with json values now 
-- templ1, res4 :: Text 
-- templ1 = "some $words$ are replaced $if(x1)$the text for x1 $x1$ $endif$."
-- -- vals1 :: M.Map Text Text 
-- vals1 = fromList vals0  -- Data.HashMap 
-- vals0 = [("words","Woerter"), ("x1","erstes x")] :: [(String,Text)]
-- -- vals2 = fromList [("words","Woerter"), ("x1","erstes x")]  
-- res4 = "some Woerter are replaced the text for x1 erstes x ."

-- test_template1 = do 
--     res <- runErr $ do 
--             tp <- compileTemplate mempty templ1
--             t :: Text <- render Nothing . renderTemplate templ1 [vals1]
--             return t
--     assertEqual (Right res4) res 
--

test_templateJson= do 
    res <- runErr $ do 
            t :: Text <- applyTemplate4 True template2 [toJSON emp1]
            return t
    assertEqual (Right "Hi, John. You make 100.\n") res 

-- for test with JSON 
data Employee = Employee { firstName :: String
                         , lastName  :: String
                         , salary    :: Maybe Int } deriving (Show)
instance ToJSON Employee where
  toJSON e = object [ "name" .= object [ "first" .= firstName e
                                       , "last"  .= lastName e ]
                    , "salary" .= salary e ]

template2 :: Text
template2 = "Hi, $name.first$. $if(salary)$You make $salary$.$else$No salary data.$endif$\n"
template3 = "$for(employee)$Hi, $employee.name.first$. $if(employee.salary)$You make $employee.salary$.$else$No salary data.$endif$$sep$\n$endfor$"
emp1 = Employee "John" "Doe" (Just 100)

data Job = Job {dept :: String, boss :: String} deriving (Show, Generic)
instance ToJSON Job

job1 = Job "Accounting" "Peter"
m2 = mergeLeftPref [toJSON emp1, toJSON job1]
template4 = "Hi, $name.first$. $if(salary)$You make $salary$.$else$No salary data.$endif$ You work for $boss$.\n"

test_templateJ2= do 
    res <- runErr $ do 
            t :: Text <- applyTemplate4 True template4 [toJSON emp1, toJSON job1]
            return t
    assertEqual (Right  "Hi, John. You make 100. You work for Peter.\n") res 
-- m2 is Object (fromList [("boss",String "Peter"),("dept",String "Accounting"),("name",Object (fromList [("first",String "John"),("last",String "Doe")])),("salary",Number 100.0)])

-- test_readWritePandoc = do 
--     res5 <- runErr $ do 
--         let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort.pandoc" 

--         let pfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort2.pandoc" 

--         pan1 :: Pandoc <- read8 pfn1 pandocFileType 
--         -- let p1 = unwrap7 pan1 :: Pandoc 

--         write8 pfn2  pandocFileType pan1
--         pan2 <- read8 pfn2 pandocFileType
--         return (pan1, pan2)
--     putIOwords ["test_readWrite", "\n res1\n", showT res5, "\n"]
--     let Right (target3, res3) = res5
--     assertEqual target3 res3


-- test_writeTexSnip2short = testVar0FileIO "uniform-pandoc" 
--         shortFile
--         "test_writeTexSnip2short" writeTexSnip4 
-- test_writeTexSnip2reg = testVar0FileIO "uniform-pandoc" 
--         regFile
--         "test_writeTexSnip2reg" writeTexSnip4 
-- test_writeTexSnip2complex = testVar0FileIO "uniform-pandoc" 
--         complexFile
--         "test_writeTexSnip2complex" writeTexSnip4 
-- test_writeTexSnip2withRef = testVar0FileIO "uniform-pandoc" 
--         withRef
--         "test_writeTexSnip2withRef" writeTexSnip4 

-- testVar0FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            -- => Text -> a -> FilePath -> (a-> ErrIO b) -> IO ()
-- writeTexSnip4 pfn1  = do       
--         pan1 :: Panrep <- read8 pfn1 panrepFileType 
--         -- let p1 = unwrap7 pan1 :: Pandoc 
--         tex1   <- writeTexSnip2 . panpan $ pan1 
--         write8 pfn1 texSnipFileType (TexSnip (panyam pan1) tex1)
--         return tex1

-- test_pdf1 = testVar0File "uniform-pandoc" shortFile 
--                 "test_writePDF2short" writePDF4 

-- writePDF4 pfn1  = do       
--         let fnin = replaceExtension pfn1 extTex 
--         let fnout = replaceExtension pfn1 extPDF 
--         -- let p1 = unwrap7 pan1 :: Pandoc 
--         res <- writePDF2 True fnin fnout 
--         putIOwords ["writePDF4 res", showT res]
--         return res
-- instance ShowTestHarness TexSnip 