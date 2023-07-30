--------------------------------------------------------------------------
--
-- Module      :  Uniform.PandocImports
-- | read and write pandoc files (intenal rep of pandoc written to disk)
-- von hier Pandoc spezifisches imortieren
-- nichts exportieren nach aussen
--
-- das ist, was von pandoc zum import gebraucht wird
-------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports   #-}

module Uniform.PandocImports
  ( module Uniform.PandocImports,
    Pandoc (..),
    Meta(..), MetaValue (..),
    Block(..)
  )
where

import Text.Pandoc
  ( 
    Meta(..),
    -- MetaValue, nullMeta,
    -- Pandoc (..),
    -- WriterOptions
    --   ( writerCiteMethod,
    --     writerExtensions,
    --     writerHighlightStyle,
    --     writerHTMLMathMethod
    --   )
    -- , def
    -- , writeLaTeX,
  )
import qualified Text.Pandoc as Pandoc
-- import Text.Pandoc 
import Text.Pandoc.Shared (stringify, addMetaField)
-- import Text.Pandoc.Shared
import Text.Pandoc.Builder 
import Uniform.Json
import UniformBase



-- | Handle possible pandoc failure within the PandocIO Monad
unPandocM :: Pandoc.PandocIO a -> ErrIO a
unPandocM op1 = callPandoc op1

callPandoc :: Pandoc.PandocIO a -> ErrIO a
callPandoc op1 = do
    res <- callIO $ Pandoc.runIO op1
    either (throwErrorT . showT) return res
  `catchError` (throwErrorT . showT)

justToKeepWarningAway :: Int 
justToKeepWarningAway = 0 




