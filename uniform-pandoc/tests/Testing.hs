{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where -- must have Main (main) or Main where

--import System.Exit

import Test.Framework
-- import     {-@ HTF_TESTS @-}       Uniform.Json_test
--import {-@ HTF_TESTS @-} Uniform.ByteString_test
-- import     {-@ HTF_TESTS @-}       Uniform.Yaml_test
--import    {-@ HTF_TESTS @-}        Uniform.FileStatus_test
--import     {-@ HTF_TESTS @-}       Uniform.Piped_test
-- import    {-@ HTF_TESTS @-}        Uniform.BibTex_test

-- import    {-@ HTF_TESTS @-}        Uniform.Markdown_test
-- import    {-@ HTF_TESTS @-}        Uniform.MetaStuff_test
import    {-@ HTF_TESTS @-}        Uniform.TemplateStuff_test
-- -- TODO not yet working inut file missing
-- import    {-@ HTF_TESTS @-}        Uniform.ProcessPDF_test
-- import {-@ HTF_TESTS @-} Uniform.Docrep_test
-- import    {-@ HTF_TESTS @-}        Uniform.HTMLout_test
-- braucht files

import UniformBase

--import TestingFileIO

test_fileio = assertBool False

main :: IO ()
main = do
  putIOwords ["HTF LayoutTest.hs:\n posTest"]
  --    htfMainWithArgs ["--quiet"] htf_importedTests
  htfMain htf_importedTests
  putIOwords ["HTF end LayoutTest.hs:\n posTest"]
  runTest test_fileio
  return ()
