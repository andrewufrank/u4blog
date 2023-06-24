----------------------------------------------------------------
--
-- Module      :  Uniform.latex
--
-- | convert latex to pdf 
---------------------------------------------------------------
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}

{-# OPTIONS_GHC -w #-}

module Uniform.Latex
  ( module Uniform.Latex
--   , writePDF2
  ) where

import Uniform.PandocImports
import Text.DocTemplates as DocTemplates
import Text.DocLayout (render)

import UniformBase
import Data.Aeson


data LatexParam = LatexParam
-- | the fields from the yaml date passed to latex-pdf
    { latTitle ::  Text  
    , latAuthor :: Text 
    , latAbstract ::  Text
    , latLanguage :: Text
    , latFn :: Text         -- ^ the original source fn
    , latBakedDir :: Text -- ^ the baked dir 
    , latDainoVersion :: Text 
    , latBibliography  :: Text  -- the bibliio file 
    , latBiblioTitle :: Text 
            -- problem with multiple files? probably not required
    , latStyle :: Text
            -- is not used 
    , latReferences :: Text  -- ^ used only for citeproc to produce html, not for biblatex to produce pdf 
    -- , latReference_section_title :: Text -- ^ the text for the title of the ref section
    -- selection by language
    -- , latBook :: Text  --  bookbig or booklet -- is   not used
    , latBookBig, latBooklet :: Text -- must be null for the non-used
    , latIndex :: IndexEntry -- of current file, copied from set up before
    , latContent :: Text -- ^ the content to fill, put at end 
    -- , latThema :: Path Abs File 
    -- , latSnips :: [IndexEntry] -- ^ the snips 
    }
    deriving (Eq, Ord, Read, Show, Generic, ToJSON)


instance Zeros LatexParam where 
    zero = LatexParam zero zero zero zero zero zero zero zero zero zero 
                zero zero zero zero zero  

-- instance FromJSON LatexParam where
--   parseJSON = genericParseJSON defaultOptions {
--                 fieldLabelModifier =  toLower' . drop 3 }

-- doclatexOptions =
--     defaultOptions
--         { fieldLabelModifier = t2s . toLowerStart . s2t . drop 2
--         }

-- instance ToJSON LatexParam where
--     toJSON = genericToJSON -- doclatexOptions - why dropping 2?

data IndexEntry = IndexEntry 
    { -- | the abs file path
      ixfn :: FilePath -- Path Abs File
    , -- | the link for this page (relative to web root)}
      link :: FilePath -- Path Rel File
    , title :: Text
    , abstract :: Text
    , author :: Text
    , date :: Text
    , content :: Text   -- in latex style, only filled bevore use
    -- , publish :: Maybe Text
    -- , indexPage :: Bool
    , dirEntries :: [IndexEntry] -- def []
    , fileEntries :: [IndexEntry] -- def []
    , headerShift :: Int   
    } deriving (Show, Read, Eq, Ord, Generic, Zeros)
    --  IndexTitleSubdirs | IndexTitleFiles 

-- instance Zeros IndexEntry where zero = IndexEntry zero zero zero zero zero zero zero zero zero

instance ToJSON IndexEntry
instance FromJSON IndexEntry

tex2latex :: NoticeLevel ->   Path Abs Dir -> LatexParam -> Path Abs File ->   ErrIO Text
-- ^ combine the latex template with the latexParam
-- the latexParam are previously filled with the content snip 
-- and the index entries 
-- needs the web root (dough dir) to find graphics

tex2latex debug   webroot latpar templFn = do 
    when (inform debug) $ putIOwords ["tex2latex start for latFn", latFn latpar]
    -- let templFn = makeAbsFile "/home/frank/Workspace11/u4blog/uniform-latex2pdf/src/Uniform/latex.dtpl"
    when (inform debug) $ putIOwords ["tex2latex template fn", showT templFn]
    -- templtxt <- readFile2 templFn
    -- putIOwords ["tex2latex template", templtxt]
    templ1<- liftIO $ compileTemplateFile (toFilePath templFn) 
    -- templ1<- liftIO $ compileTemplate mempty templ 
    let templ3 = case templ1 of
            Left msg -> errorT ["applyTemplate4 error", s2t msg]
            Right tmp2 -> tmp2
    -- let latpar2 = latpar{latContent = snip}   already filled     
    let latparJ = toJSON latpar
    when (inform debug) $ putIOwords ["tex2latex latparJ", showT latparJ]
    let doc1 =  renderTemplate templ3 latparJ
    let doc2 = render Nothing doc1
    when (inform debug) $ putIOwords ["tex2latex result",  doc2]
    return doc2


latexLangConversion :: Text -> Text 
latexLangConversion inlang = 
    case lang2 of 
        "de" -> "ngerman"
        "en" -> "english"
        _ -> "english"
    where 
        lang2 = take' 2 inlang


-- https://tex.stackexchange.com/questions/82993/how-to-change-the-name-of-document-elements-like-figure-contents-bibliogr
-- for an automatic adaption based on the language
-- \renewcaptionname{ngerman}{\contentsname}{Inhalt}           %Table of contents
-- \renewcaptionname{ngerman}{\listfigurename}{Abbildungen}    %Figures
-- \renewcaptionname{ngerman}{\listtablename}{Tabellen}        %Tables
-- \renewcaptionname{ngerman}{\figurename}{Abb.}               %Figure
-- \renewcaptionname{ngerman}{\tablename}{Tab.}                %Table
-- \renewcaptionname{ngerman}{\bibname}{Literatur}             %Bibliography
--   \newcaptionname{ngerman}{\lstlistlistingname}{Quelltexte} %Table of listings 
--   \newcaptionname{ngerman}{\lstlistingname}{Quelltext}      %Listing


