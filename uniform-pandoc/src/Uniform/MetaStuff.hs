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

module Uniform.MetaStuff
  ( module Uniform.MetaStuff,
    addListOfDefaults
    , readMd2pandoc
    , md2Meta_Process    
    , Pandoc (..)
    , Meta(..)
    , setValue2meta
    , getValue4meta
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
import Text.Pandoc.Shared (stringify, addMetaField)
import Text.Pandoc.Builder (ToMetaValue) -- do not import more!
import Uniform.Json
    (  FromJSON(parseJSON),
      Value,
      ToJSON(toJSON, toJSONList),
      parseMaybe ) 
import Uniform.PandocImports 
import Uniform.PandocImports as Pandoc
    ( Meta(Meta))
    
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
import Data.Map ( fromList, toList) 
import Text.Pandoc.Definition as Pandoc
    ( 
      Pandoc(..),
      lookupMeta,
      MetaValue (..),
      nullMeta,
      Inline(Str) )
import Text.Pandoc.Writers.Shared as Pandoc
import Uniform.Markdown  
import qualified Text.Pandoc.Citeproc as PC
import Uniform.HttpFiles
import Uniform.TemplateStuff
import Text.DocTemplates as DocTemplates ( Doc )
import Uniform.PandocHTMLwriter

------------ settings (copied to avoid circular import)

data Settings = Settings
    { ---  siteLayout ::  
      localhostPort :: Int 
    , settingsAuthor :: Text 
    , settingsDate :: Text -- should be UTC 
    , siteHeader :: SiteHeader 
    , menu :: [MenuItem]
    -- , today :: Text
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)

instance ToJSON Settings
instance FromJSON Settings

data SiteHeader = SiteHeader 
    { sitename :: FilePath 
    , byline :: Text 
    , banner :: FilePath 
    -- , bannerCaption :: Text 
    } deriving (Show, Read, Ord, Eq, Generic, Zeros)
instance ToJSON SiteHeader
instance FromJSON SiteHeader

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

instance Zeros (M.Map Text Text) where zero = fromList []

data ExtraValues = ExtraValues 
                        { dainoVersion:: Text
                        , bakedDir :: Text
                        }
    deriving (Eq, Ord, Show, Read, Generic)
    
instance ToJSON ExtraValues 
instance FromJSON ExtraValues 

instance Zeros ExtraValues where zero = ExtraValues zero zero 

-- 3 functions for md2docrep : 

readMd2pandoc :: Path Abs File -> ErrIO Pandoc 
-- read an mdFile to Pandoc 
readMd2pandoc fn = do 
        mdfile <- read8 fn markdownFileType
        pd <- readMarkdown2 mdfile
        return pd 

addListOfDefaults :: [(Text, Text)] -> Pandoc -> Pandoc 
-- add a list of default key value pairs to a Meta
-- if a key is present, its value is retainied 
-- defaults are set only if key not present 
addListOfDefaults list (Pandoc m1 p1 ) = Pandoc m2 p1 
    where
        m2 = Meta   $ M.union metavals (fromList defvals) 
        metavals =  unMeta $ m1 
        defvals = map (second MetaString) list

md2Meta_Process :: Pandoc -> ErrIO Meta
-- process the pandoc file with citeproc 
-- stores the result as body in meta
-- the body is not required anymore!
md2Meta_Process  pandoc1 = do
    (Pandoc m1 p1) <- unPandocM $ PC.processCitations pandoc1
    let m2 = Meta $ M.insert "body" (MetaBlocks p1) (unMeta m1)

    -- let c1 = Meta (fromList [(("body"::Text), (MetaBlocks p1))])
    -- -- todo missing the index, references, umlaut conversion
    --     cn = mergeAll [m1, c1] :: Meta-- order may  be important
    -- -- putIOwords ["md2Meta cn \n", showT cn, "\n--"]
    return m2

-- 

meta2hres :: Template Text -> Meta -> ErrIO HTMLout
-- step2: the second part resulting in HTML result
meta2hres templH meta = do
    -- putIOwords ["meta2hres meta \n", showT meta, "\n--"]
    -- convert to list of (text,Block) 
    -- make M.Map and pass to render template 

    tHtml :: M.Map Text Text <- meta2xx  writeHtml5String2 meta
    -- putIOwords ["meta2hres tHtml \n", showT tHtml, "\n--"]

    -- templH :: Template Text <- compileDefaultTempalteHTML
        -- templL :: Template Text  <-compileDefaultTempalteLatex
        -- -- renderTemplate :: (TemplateTarget a, ToContext a b) => Template a -> b -> Doc a
    let restplH = renderTemplate templH tHtml :: Doc Text
    let resH = render (Just 50) restplH  :: Text  -- line length, can be Nothing
        -- let restplL = renderTemplate templL ctLatex :: Doc Text
        -- let resL = render (Just 50) restplL  :: Text  -- line length, can be Nothing    -- todo 
    return (HTMLout resH)

--  functions to extract values from the meta record 
-- containing the YAML Header values

getDoNotReplace :: Pandoc -> Maybe MetaValue
-- get the string in the Yaml with words which must be preserved
-- a comma separated list 
getDoNotReplace pan =
        lookupMeta "DoNotReplace" (getMeta pan)

getFromYaml  :: Text -> Pandoc -> Maybe MetaValue
-- get the MetaValue in the Yaml  at the key
getFromYaml key pan =
        Pandoc.lookupMeta key (getMeta pan)

getMetaValueFromYaml4 :: Text -> Text -> Pandoc ->  MetaValue
--get the Metavalue (with  default)
getMetaValueFromYaml4 def1 key pan = fromMaybe (MetaString def1)
        $ getFromYaml key pan

getTextFromYaml5 :: Text -> Text -> Pandoc ->  Text
--get the Metavalue (with  default)
getTextFromYaml5 def1 key pan =
    fromJustNoteT ["getTextFromYaml5"::Text, key] .
    metaValueToText . fromMaybe (MetaString def1)
        $ getFromYaml key pan

metaValueToText :: MetaValue -> Maybe Text
metaValueToText (MetaString t) = Just t
metaValueToText (MetaInlines ils) = Just $ stringify ils
metaValueToText (MetaBlocks bls) = Just $ stringify bls
metaValueToText (MetaList xs) = unwords' <$> mapM metaValueToText xs
metaValueToText _ = Nothing

metaValueToBlock :: MetaValue -> Maybe [Block]
metaValueToBlock (MetaString t) = Just . sing $ Plain [Str t]
metaValueToBlock (MetaBool t) = Just . sing $ Plain [Str . showT $ t]
metaValueToBlock (MetaInlines ils) = Just . sing$ Plain ils  -- could be Para
metaValueToBlock (MetaBlocks bls) = Just $ bls
metaValueToBlock (MetaList xs) = Just $ mapmb xs
    where 
        mapmb :: [MetaValue] -> [Block]
        mapmb xs2 =   map mbBlocks xs2
        mbBlocks :: MetaValue -> Block
        mbBlocks (MetaInlines ils) =    Plain $ ils 
        mbBlocks x = errorT  ["mbBlocks", showT x]
    -- unwords' <$> mapM metaValueToText xs
metaValueToBlock _ = Nothing
sing a = [a]


-- block2htmltext  :: [Block] -> ErrIO Text
-- -- convert a Block to a html text 
-- block2htmltext b = writeHtml5String2 (Pandoc nullMeta b)

-- metaValueToHTML :: MetaValue -> ErrIO Text
-- metaValueToHTML mv = do 
--     let bs = metaValueToBlock mv ::Maybe [Block]
--     t <- block2htmltext (fromJustNote "metaValueToHTML" $ bs)
--     return t

block2xx  :: (Pandoc -> ErrIO Text) ->[Block] -> ErrIO Text
-- convert a Block to a html text 
block2xx writeXX b = writeXX (Pandoc nullMeta b)

metaValue2xx :: (Pandoc -> ErrIO Text) -> MetaValue -> ErrIO Text
metaValue2xx writer mv = do 
    let bs = metaValueToBlock mv ::Maybe [Block]
    t <- block2xx writer (fromJustNote ("metaValueTo2xx " <> show mv) $ bs)
    return t

meta2xx :: (Pandoc -> ErrIO Text) -> Meta -> ErrIO (M.Map Text Text)
-- convert all in Meta to   codes (html or latex depending on writer)
meta2xx writer  m1 = do
    let listMetaValues = toList . unMeta $ m1:: [(Text, MetaValue)]
    l2 <- mapM mapSec listMetaValues
    return $ fromList l2
  where 
    mapSec :: (Text, MetaValue) -> ErrIO (Text, Text)
    mapSec (t, mv) = do  
        mv2 :: Text <- metaValue2xx writer mv
        return (t, mv2) -- fromJustNote "meta2htmltext" $ mv2)

md2Meta :: Path Abs File -> MarkdownText -> ErrIO Meta
-- step1: convert a markdown file to MetaValue
-- independent of output target (html or latex)
md2Meta  filep mdtext = do
    putIOwords ["md2Meta filepath", showT  filep, "\n--"] 
    p1 <- md2Meta_Readmd filep mdtext
    p2 <- md2Meta_Process p1 
    return p2
    -- broken in two to be able to insert the default values 
    -- into the metadata before processing the cites
  

md2Meta_Readmd :: Path Abs File -> MarkdownText -> ErrIO Pandoc
-- step1:reads only the md file to pandoc 
md2Meta_Readmd  filep mdtext = do
    putIOwords ["md2Meta filepath", showT  filep, "\n--"] 
    pandoc1<- readMarkdown2 mdtext
    return pandoc1
 

mergeAll :: [Meta] -> Meta
-- combines list, preference to the right (last key wins if duplicated!)
mergeAll  = Meta . fromList . concat . map toList . map unMeta

meta2pandoc :: Meta -> Pandoc
-- create a pandoc from a meta with an empty block
-- useful to apply writer to meta
meta2pandoc m = Pandoc m []

addMetaFieldT ::   Text -> Text -> Meta -> Meta
-- better not use:
-- if a value exists, converts to list and adds
addMetaFieldT = addMetaField

getValue4meta :: Meta -> Text -> Text 
-- get a value from Meta which is a MetaString
-- null when not set 
getValue4meta m t = Pandoc.lookupMetaString t m

setValue2meta ::  Text -> Text -> Meta -> Meta
--set a value as MetaString, overwrites existing value
-- not usable for setting defaults! 
setValue2meta k v m = Meta (M.insert k (MetaString v) (unMeta m))

addMetaField2pandoc :: ToMetaValue a => Text -> a -> Pandoc -> Pandoc
addMetaField2pandoc key val (Pandoc m b) = Pandoc m2 b
    where   m2 = addMetaField key val m


-- writeLaTeX2 ::    Pandoc -> ErrIO Text
-- -- gives a texsnip Text
-- writeLaTeX2 pan = unPandocM $ writeLaTeX latexOptions pan 

instance Zeros Pandoc where
  zero =  Pandoc nullMeta zero

instance Zeros Pandoc.Meta where
  zero = mempty

getMeta :: Pandoc -> Pandoc.Meta
getMeta (Pandoc.Pandoc m _) = m

putMeta :: Pandoc.Meta -> Pandoc -> Pandoc
putMeta m1 (Pandoc _ p0) = Pandoc m1 p0

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

-- | Flatten a Pandoc 'Meta' into a well-structured JSON object,  
-- adapted from https://hackage.haskell.org/package/slick-1.1.1.0/docs/src/Slick.Pandoc.html#flattenMeta
-- does squish bulleted list and other markdown in YAML
-- metaValueToText seems better
flattenMeta :: Pandoc.Meta -> Value
flattenMeta (Pandoc.Meta meta) = toJSON $ fmap go meta
  where
    go :: MetaValue -> Value
    go (Pandoc.MetaMap m) = toJSON $ fmap go m
    go (Pandoc.MetaList m) = toJSONList $ fmap go m
    go (Pandoc.MetaBool m) = toJSON m
    go (Pandoc.MetaString m) = toJSON m
    go (Pandoc.MetaInlines m) = toJSON $ stringify m
    go (Pandoc.MetaBlocks m) = toJSON $ stringify m

-- readYaml2value :: Path Abs File -> ErrIO Value
-- -- | read a yaml file to a value
-- -- error when syntax issue
-- readYaml2value fp = do
--   t <- read8 fp yamlFileType
--   return . yaml2value $ t



