---------------------------------------------------------------------
--
-- Module      :  Uniform.Yaml
-- TOD separate markdown
----------------------------------------------------------------------
-- {-# LANGUAGE BangPatterns                   #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances #-}

module Uniform.Yaml
  ( module Uniform.Yaml,
    -- module Uniform.Error, -- or at least 
    ErrIO,
    -- , Y.decodeEither'
    Y.ParseException (..),
    -- , module Data.Yaml
    Y.decodeFileThrow,
    Y.encode,
    Y.decode,
    Y.decodeEither,
    Options(..)
  )
where

-- import Path -- (Path, Abs, Rel, File, Dir)
-- import Uniform.FileIO (read8, Extension(..))
import Data.Aeson.Types (Options(..))

import qualified Data.Yaml as Y
import UniformBase
-- import Uniform.Error
-- import Unifor.Strings
-- import Uniform.FileIO
import Uniform.Json
-- import Uniform.TypedFile (TypedFile5 (..), TypedFiles7 (..))

decodeThrowT :: Text -> ErrIO Value
decodeThrowT = Y.decodeThrow . t2b

newtype YamlText = YamlText Text deriving (Show, Read, Eq, Ord)

-- a wrapper around Markdonw text
-- todo clean up - use wrap7
unYAML :: YamlText -> Text
unYAML (YamlText a) = a --needed for other ops

extYAML :: Extension
extYAML = Extension "yaml"

yamlFileType :: TypedFile5 Text YamlText

instance Zeros YamlText where zero = YamlText zero

yamlFileType = TypedFile5 {tpext5 = extYAML} :: TypedFile5 Text YamlText

--instance FileHandles YamlText
-- what is missing here?

instance TypedFiles7 Text YamlText where
  -- handling Markdown and read them into YamlText
  wrap7 = YamlText
  unwrap7 (YamlText a) = a

readYaml2value :: Path Abs File -> ErrIO Value
-- read a yaml file to a value
-- error when syntax issue
readYaml2value fp = do
  t <- read8 fp yamlFileType
  return . yaml2value $ t

yaml2value :: YamlText -> Value
-- convert a YamlText to a JSON value, error if not ok
-- how to debug input erros?
yaml2value yt = either (error . show) Prelude.id vx
  where
    vx = Y.decodeEither' (t2b . unYAML $ yt) :: Either Y.ParseException Value

readYaml2rec :: (FromJSON a, Show a) => Path Abs File -> ErrIO a 
-- | read a yaml file into a record Value 
-- error when syntax fault
readYaml2rec fn = do 
    -- putIOwords [" yaml file name", showT fn ]
    -- settingsTxt <- read8 settingsfilename yamlFileType
    s0 :: Value <- readYaml2value fn 
    -- putIOwords ["yaml read", showPretty s0 ]

    s1  <-  fromJSONerrio  s0  -- :: Result Settings 

    -- putIOwords ["json parsed", showT s1 ]

    return s1
-- for a test use readSettingsfile