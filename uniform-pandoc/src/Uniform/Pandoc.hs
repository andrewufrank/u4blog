--------------------------------------------------------------------------
--
-- Module      :  Uniform.Pandoc
        -- top import, darf nicht von andern importiert werden hier 
-------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-unused-imports 
            #-}

module Uniform.Pandoc
  ( module Uniform.Pandoc 

  , module Uniform.PandocImports
    , ReaderOptions
  , module Uniform.PandocHTMLwriter
  , module Uniform.BibTex
  , 
  )
where

import Uniform.PandocImports
import Uniform.PandocHTMLwriter
import Uniform.BibTex

import qualified Text.Pandoc                   as Pandoc
import Text.Pandoc ( ReaderOptions, WriterOptions(..) )        
 
justToKeepWarningAway :: Int 
justToKeepWarningAway = 0 