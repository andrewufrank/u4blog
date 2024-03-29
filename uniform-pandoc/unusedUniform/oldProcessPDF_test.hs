---------------------------------------------------------------------------
--
-- Module      :  process pdf  test
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

module Uniform.ProcessPDF_test where
--
import Test.Framework
---- using uniform:
import Uniform.Test.TestHarness
import Uniform.Pandoc
import Uniform.ProcessPDF 
import Uniform.Markdown_test 
import Uniform.Filetypes4sites

import UniformBase

-- cannot read or write pdf files ?
-- read of a pdf (displays ok) gets invalid argument (invalid byte sequence)

-- test_readWriteLatex = do 
--     res4 <- runErr $ do 
--         let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort" 
--         let pfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShortRWtest" 

--         pan1 <- read8 pfn1 texFileType 
--         write8 pfn2  texFileType pan1

--         pan2 <- read8 pfn2 texFileType
--         return (pan1, pan2)
--     -- putIOwords ["test_readWriteLatex", "\n res1\n", showT res4, "\n"]
--     let Right (target3, res3) = res4
--     assertEqual target3 res3


test_text2latex4short = testVar0FileIO "uniform-pandoc" 
        shortFile
        "test_text2latex4short" (text2latex4 zero) 
test_text2latex4reg = testVar0FileIO "uniform-pandoc" 
        regFile
        "test_text2latex4reg" (text2latex4 zero) 
test_text2latex4complex = testVar0FileIO "uniform-pandoc" 
        complexFile
        "test_text2latex4complex" (text2latex4 zero) 
test_text2latex4withRef = testVar0FileIO "uniform-pandoc" 
        withRef
        "test_text2latex4withRef" (text2latex4 latexParam) 

latexParam = LatexParam {latBibliography = Just "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/resources/BibTexLatex.bib"
        , latStyle = Just "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/resources/chicago-fullnote-bibliography-bb.csl"}


-- testVar0FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            -- => Text -> a -> FilePath -> (a-> ErrIO b) -> IO ()

text2latex4 latexParam tsfn1  = do       
    texsn1  <- read8 tsfn1 texSnipFileType   -- does include json data 
    -- let p1 = unwrap7 pan1 :: Pandoc 
    -- let latexParam = LatexParam {latBibliography = Just "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/resources/BibTexLatex.bib"
    --     , latStyle = Just "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/resources/chicago-fullnote-bibliography-bb.csl"}
    -- putIOwords ["text2latex4", showT latexParam]
    let lat1 =  tex2latex latexParam [texsn1] 
    write8 tsfn1 texFileType lat1  
    return lat1

instance ShowTestHarness Latex 
 

-- test_writePDF4short = testVar0FileIO "uniform-pandoc" 
--         shortFile
--         "test_writePDF4short" writePDF4 
-- test_writePDF4reg = testVar0FileIO "uniform-pandoc" 
--         regFile
--         "test_writePDF4reg" writePDF4 
-- to here commented out

-- test_writePDF4complex = testVar0FileIO "uniform-pandoc" 
--         complexFile
--         "test_writePDF4complex" writePDF4 
test_writePDF4complex = testVar0FileIO "uniform-pandoc" 
        withRef
        "test_writePDF4withRef" writePDF4 

writePDF4 tsfn1  = do       
        pan1  <- read8 tsfn1 texFileType 
        -- let p1 = unwrap7 pan1 :: Pandoc 
        let cwd1 = makeAbsDir . getParentDir $ tsfn1 :: Path Abs Dir 
        writePDF2 True tsfn1 tsfn1 cwd1
        -- writes the pdf file
        -- pdf1 <- read8 tsfn1 pdfFileType  -- read8 cannot read pdf
        return . PDFfile $ "ok"

-- test_panrep2texsnipShort = testVar0FileIO "uniform-Docrep" 
--         shortFile
--         "test_panrep2texsnipShort" panrep2texsnipTest 
-- test_panrep2texsnipReg = testVar0FileIO "uniform-Docrep" 
--         regFile
--         "test_panrep2texsnipReg" panrep2texsnipTest 
-- test_panrep2texsnipComplex = testVar0FileIO "uniform-Docrep" 
--         complexFile
--         "test_panrep2texsnipComplex" panrep2texsnipTest 
-- test_panrep2texsnipWithRef = testVar0FileIO "uniform-Docrep" 
--         withRef
--         "test_panrep2texsnipWithRef" panrep2texsnipTest 

-- -- withRef = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/withRef.md"
-- -- defined in markdown_test.hs

-- panrep2texsnipTest :: Path Abs File -> ErrIO TexSnip
-- panrep2texsnipTest drfn  = do       
--     pr1 :: Panrep <- read8 drfn panrepFileType 
--     res1 :: TexSnip <-  panrep2texsnip  pr1 
--     write8 drfn texSnipFileType (TexSnip (panyam pr1) res1)
--     return res1
    
instance ShowTestHarness PDFfile