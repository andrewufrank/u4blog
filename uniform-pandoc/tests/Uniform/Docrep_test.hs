------------------------------------------------------------------------
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
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.Docrep_test where

import Test.Framework
    ( assertEqual_, makeLoc, makeTestSuite, makeUnitTest, TestSuite )
import           Uniform.Pandoc
import Uniform.Docrep ()
import Uniform.Json
    ( encode,
      object,
      fromList,
      Value(String, Object),
      (.=),
      AtKey(putAtKey) )
import UniformBase ()
import Uniform.Test.TestHarness
    ( assertEqual_,
      makeLoc,
      makeTestSuite,
      makeUnitTest,
      TestSuite,
      testVar0FileIO,
      ShowTestHarness )
-- import Text.Pandoc 
-- import           Uniform.Error           hiding ( (<.>) )  -- (</>)
import Uniform.Markdown_test
    ( shortFile, regFile, complexFile, withRef )
import Uniform.Filetypes4sites


test_zero = (assertEqual_ (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 45))
    "Docrep {yam = Null, pan = Pandoc (Meta {unMeta = fromList []}) []}"
    (showT (zero :: Docrep))

test_readWriteDR = do
    res4 <- runErr $ do
        let
            pfn1 =
                makeAbsFile
                    "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort"
        let
            pfn2 =
                makeAbsFile
                    "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort2"

        pan1 <- read8 pfn1 docrepFileType
        write8 pfn2 docrepFileType pan1

        pan2 <- read8 pfn2 docrepFileType
        return (pan1, pan2)
    putIOwords ["test_readWrite", "\n res1\n", showT res4, "\n"]
    let Right (target3, res3) = res4
    (assertEqual_ (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 67)) target3 res3

-- this is for json (not aeson)
test_json :: IO ()
test_json = (assertEqual_ (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 71)) res1a (encode rec1json)
rec1json :: Value
rec1json = Object
    (fromList
        [ ("date" , String "2000-01-01T00:00:00Z")
        , ("title", String "rec1")
        , ("date2", String "2000-01-01T00:00:00Z")
        ]
    )
-- res1 :: bytestring-0.10.12.0:Data.ByteString.Lazy.Internal.ByteString
-- result in any order is acceptable 
res1 =
    "{\"date\":\"2000-01-01T00:00:00Z\",\"title\":\"rec1\",\"date2\":\"2000-01-01T00:00:00Z\"}"
res1a = "{\"title\":\"rec1\",\"date\":\"2000-01-01T00:00:00Z\",\"date2\":\"2000-01-01T00:00:00Z\"}"

-- for aeson  see https://artyom.me/aeson
v1 = "Object (fromList [(\"boolean\",Bool True),(\"numbers\",Array [Number 1.0,Number 2.0,Number 3.0])])"
val :: Value
val = object [
  "boolean" .= True,
  "numbers" .= [1,2,3::Int] ]

test_DR1 = (assertEqual_ (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 93)) dr1t $ showT  dr1
dr1 = Docrep val zero
dr1t = "Docrep {yam = Object (fromList [(\"boolean\",Bool True),(\"numbers\",Array [Number 1.0,Number 2.0,Number 3.0])]), pan = Pandoc (Meta {unMeta = fromList []}) []}" :: Text

-- a11 =  Array [Number 1.0, Number 2.0, Number 3.0] :: Value    
-- test_encode :: IO ()
-- test_encode = assertEqual res2 $ Docrep (encode rec1json) zero
-- res2 = Docrep zero zero

-- -- res1 :: Data.ByteString.Lazy.Internal.ByteString

-- test_DRencode = assertEqual res2 $ 

test_setText = (assertEqual_ (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 106)) rec2a
        $ showT $ putAtKey ("t1"::Text) (22::Integer) val
rec2 = "Object (fromList [(\"boolean\",Bool True),(\"t1\",Number 22.0),(\"numbers\",Array [Number 1.0,Number 2.0,Number 3.0])])" :: Text
rec2a = "Object (fromList [(\"boolean\",Bool True),(\"numbers\",Array [Number 1.0,Number 2.0,Number 3.0]),(\"t1\",Number 22.0)])"
test_set2dr1 = (assertEqual_ (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 110)) rec3 $ showT . putAtKey ("a2"::Text) ("testa2"::Text) $ (dr1::Docrep)
rec3 = "Docrep {yam = Object (fromList [(\"a2\",String \"testa2\"),(\"boolean\",Bool True),(\"numbers\",Array [Number 1.0,Number 2.0,Number 3.0])]), pan = Pandoc (Meta {unMeta = fromList []}) []}"::Text

dr3 = putAtKey ("a2"::Text) ("testa2"::Text) (dr1::Docrep)
val3 = [object [
  "boolean2" .= False,
  "numbers" .= [4,44::Int] ]
  , object [
  "b4" .= ("b4test"::Text),
  "numbs" .= [66,55,44::Int] ]  ]
test_merge1 = (assertEqual_ (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 120)) rec4a $ showT $ mergeAll dr3 val3
rec4 = "Docrep {yam = Object (fromList [(\"a2\",String \"testa2\"),(\"boolean\",Bool True),(\"numbs\",Array [Number 66.0,Number 55.0,Number 44.0]),(\"numbers\",Array [Number 4.0,Number 44.0]),(\"boolean2\",Bool False),(\"b4\",String \"b4test\")]), pan = Pandoc (Meta {unMeta = fromList []}) []}"::Text
rec4a = "Docrep {yam = Object (fromList [(\"a2\",String \"testa2\"),(\"b4\",String \"b4test\"),(\"boolean\",Bool True),(\"boolean2\",Bool False),(\"numbers\",Array [Number 4.0,Number 44.0]),(\"numbs\",Array [Number 66.0,Number 55.0,Number 44.0])]), pan = Pandoc (Meta {unMeta = fromList []}) []}"



test_panrep2htmlShort = testVar0FileIO "uniform-Docrep"
        shortFile
        "test_panrep2htmlShort" panrep2htmlTest
test_panrep2htmlReg = testVar0FileIO "uniform-Docrep"
        regFile
        "test_panrep2htmlReg" panrep2htmlTest
test_panrep2htmlComplex = testVar0FileIO "uniform-Docrep"
        complexFile
        "test_panrep2htmlComplex" panrep2htmlTest
test_panrep2htmlWithRef = testVar0FileIO "uniform-Docrep"
        withRef
        "test_panrep2htmlWithRef" panrep2htmlTest


panrep2htmlTest :: Path Abs File -> ErrIO HTMLout
panrep2htmlTest drfp = do
    pr1 <- read8 drfp panrepFileType
    h1 <- panrep2html pr1
    write8 drfp htmloutFileType h1
    return h1

-- test addRefs -------------
addRefs2Test :: Path Abs File -> ErrIO Docrep
addRefs2Test drfp = do
    dr1 <- read8 drfp docrepFileType
    dr2 <- addRefs2 dr1 biblio
    return dr2

biblio = "resources/BibTexLatex.bib"
test_addRefsTest =  testVar0FileIO "uniform-Docrep"
        withRef  -- withRef has only content, no biblio
        "addRefsTest" addRefs2Test


instance ShowTestHarness TexSnip
instance ShowTestHarness HTMLout

htf_Uniform_Docrep_test_thisModulesTests :: TestSuite
htf_Uniform_Docrep_test_thisModulesTests = makeTestSuite "Uniform.Docrep_test" [ "zero" (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 45) test_zero,



    makeUnitTest "readWriteDR" (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 49) test_readWriteDR,





















    makeUnitTest "json" (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 71) test_json,





















    makeUnitTest "DR1" (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 93) test_DR1,












    makeUnitTest "setText" (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 106) test_setText,



    makeUnitTest "set2dr1" (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 110) test_set2dr1,









    makeUnitTest "merge1" (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 120) test_merge1,





    makeUnitTest "panrep2htmlShort" (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 126) test_panrep2htmlShort,


    makeUnitTest "panrep2htmlReg" (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 129) test_panrep2htmlReg,


    makeUnitTest "panrep2htmlComplex" (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 132) test_panrep2htmlComplex,


    makeUnitTest "panrep2htmlWithRef" (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 135) test_panrep2htmlWithRef,



















    makeUnitTest "addRefsTest" (makeLoc "/home/frank/Workspace11/u4blog/uniform-pandoc/tests/Uniform/Docrep_test.hs" 155) test_addRefsTest
  ]

