-----------------------------------------------------------------------------
--
-- Module      :  Uniform.latex2pdf_test
--
-- | a miniaml set of  
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- {-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# LANGUAGE UndecidableInstances  #-}
-- {-# LANGUAGE DeriveGeneric  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module Uniform.Latex2pdf_test where

import           Test.Framework
import           UniformBase
import Uniform.Latex         

test_null2 = 
            assertEqual 0 0 
    
-- tex2latex_files fn = do 
--     do 
--     r <- runErrOrVal $ do 
 