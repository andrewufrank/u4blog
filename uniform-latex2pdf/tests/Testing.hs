{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
----------------------------------------------------------------------
--
-- Module      :   top tests for pdf
-------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Uniform.Latex2pdf_test
import Uniform.Latex

import UniformBase

main = do
  putIOwords ["HTF watchTest.hs:\n uniform-watch test"]
  r <- htfMainWithArgs ["--quiet"] htf_importedTests
  putIOwords ["HTF end examp;eTest.hs:\n", showT r]

  return r

