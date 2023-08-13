--------------------------------------------------------------------------
--
-- Module      :  Uniform.MetaPlus 
-- | the record in which all site specific data are collected
-- and later taken into the output files 
-- must have a toJSON to work with templates
-------------------------------
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass          #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports   #-}

module Uniform.MetaPlus
  ( module Uniform.MetaPlus
    , Meta(..)
  )
where

-- import Text.Pandoc
--   (
--     Meta(..),
--     -- MetaValue, nullMeta,
--     -- Pandoc (..),
--     -- WriterOptions
--     --   ( writerCiteMethod,
--     --     writerExtensions,
--     --     writerHighlightStyle,
--     --     writerHTMLMathMethod
--     --   )
--     -- , def
--     -- , writeLaTeX,
--   )
-- import qualified Text.Pandoc as Pandoc
-- import Text.Pandoc.Shared (stringify, addMetaField)
-- import Text.Pandoc.Builder (ToMetaValue) -- do not import more!
import Uniform.Json
--     (  FromJSON(parseJSON),
--       Value,
--       ToJSON(toJSON, toJSONList),
--       parseMaybe ) 
import Uniform.PandocImports 
-- import Uniform.PandocImports as Pandoc
--     ( Meta(Meta))
    
    --   MetaValue(MetaBlocks, MetaMap, MetaList, MetaBool, MetaString,
                -- MetaInlines),
    --   Pandoc(Pandoc) ) 
  
import UniformBase
-- import Uniform.PandocImports
--     ( unPandocM,
--       Block(Plain),
--       Meta(..),
--       MetaValue(MetaList, MetaString, MetaBool, MetaInlines, MetaBlocks),
--       Pandoc(..) )  
import qualified Data.Map as M 
-- import Data.Map ( fromList, toList) 
import Text.Pandoc.Definition as Pandoc
--     ( 
--       Pandoc(..),
--       lookupMeta,
--       MetaValue (..),
--       nullMeta,
--       Inline(Str) )
-- import Text.Pandoc.Writers.Shared as Pandoc
-- import Uniform.Markdown  
-- import qualified Text.Pandoc.Citeproc as PC
-- import Uniform.HttpFiles
-- import Uniform.TemplateStuff
-- import Text.DocTemplates as DocTemplates ( Doc )
-- import Uniform.PandocHTMLwriter

------------ settings (copied to avoid circular import)

data Settings = Settings
    { ---  siteLayout ::  
      localhostPort :: Int 
    , settingsAuthor :: Text 
    , settingsDate :: Text -- should be UTC 
    , siteHeader :: SiteHeader 
    , menu :: MenuItems
    -- , today :: Text
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)

instance ToJSON Settings
instance FromJSON Settings

data SiteHeader = SiteHeader 
    { sitename :: FilePath 
    , byline :: Text 
    , banner :: FilePath 
    , bannerCaption :: Text 
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON SiteHeader
instance FromJSON SiteHeader


newtype MenuItems = MenuItems {menuNav:: [MenuItem]
                            -- , menuB:: Text
                            } deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON MenuItems 
instance FromJSON MenuItems 


data MenuItem = MenuItem  
    { navlink :: FilePath 
    , navtext :: Text
    -- , navpdf :: Text  -- for the link to the pdf 
    -- not a good idead to put here
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON MenuItem
instance FromJSON MenuItem

data MetaPlus = MetaPlus 
                { metap :: Meta    -- ^ the pandoc meta 
                , sett :: Settings -- ^ the data from the settingsfile
                , extra :: ExtraValues -- ^ other values to go into template
                , metaMarkdown :: M.Map Text Text 
                , metaHtml ::  M.Map Text Text
                , metaLatex ::  M.Map Text Text
                }
    deriving (Eq, Ord, Show, Read, Generic) -- Zeros, ToJSON, FromJSON)
instance ToJSON MetaPlus
instance FromJSON MetaPlus
instance Zeros MetaPlus where 
        zero = MetaPlus zero zero zero zero zero zero
instance PrettyStrings MetaPlus where 
    showPretty = s2t . ppShow
-- instance Show MetaPlus where
--     show = ppShow 

instance Zeros (M.Map Text Text) where zero = M.fromList []

-- the extraValues will eventually go into settings
data ExtraValues = ExtraValues 
                        { extraDainoVersion:: Text
                        , extraBakedDir :: Text
                        }
    deriving (Eq, Ord, Show, Read, Generic)
    
instance ToJSON ExtraValues 
instance FromJSON ExtraValues 

instance Zeros ExtraValues where zero = ExtraValues zero zero 

instance Zeros Pandoc where
  zero =  Pandoc nullMeta zero

instance Zeros Pandoc.Meta where
  zero = mempty