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

{-# OPTIONS_GHC -w #-}

module Uniform.Latex
  ( module Uniform.Latex
  , writePDF2
  ) where

import qualified System.Exit as Sys
import qualified System.Process as Sys
import System.IO.Silently (silence)
import Uniform.Json
import UniformBase
import Uniform.WritePDF 
-- import Data.Aeson
-- import Data.Aeson.Types


data LatexParam = LatexParam
-- | the fields from the yaml date passed to latex-pdf
    { latTitle ::  Text  
    , latAuthor :: Text 
    , latAbstract ::  Text
    , latLanguage :: Text
    , latBibliography  :: Text  -- the bibliio file 
            -- problem with multiple files? 
    , latStyle :: Text
            -- is not used 
    , latReferences :: Text  -- ^ used only for citeproc to produce html, not for biblatex to produce pdf 
    -- , latReference_section_title :: Text -- ^ the text for the title of the ref section
    -- selection by language
    , latBook :: Bool  -- is this a long text for a book/booklet
    , latContent :: [Text] -- ^ a list of the .md files which are collected into a multi-md pdf
    }
    deriving (Eq, Ord, Read, Show, Generic)

instance Zeros LatexParam where 
    zero = LatexParam zero zero zero zero zero zero zero zero zero

instance FromJSON LatexParam where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier =  toLower' . drop 3 }

doclatexOptions =
    defaultOptions
        { fieldLabelModifier = t2s . toLowerStart . s2t . drop 2
        }

instance ToJSON LatexParam where
    toJSON = genericToJSON doclatexOptions

-- TODO create a default/minimal preamble 
-- and add preamble as parameter

tex2latex :: Path Abs Dir -> LatexParam ->  Text -> Text
-- ^ combine a snipped (produced from an md file) with a preamble to
--  produce a compilable latex file.
--  references are processed earlier (in  panrep)
--  keps the metadata
-- needs the web root (dough dir) to find graphics

tex2latex webroot latpar snips = concat'
        $ [ unlines' 
                (preamble1 webroot
                    -- (maybe ""  (s2t . toFilePath) $ latBibliographyP latpar)
                    -- (maybe "authoryear" id (latStyle latpar))
                    latpar
                )
          , snips -- concat' snips -- (map unTexSnip snips)
          , unlines' $
                -- if isNothing (latBibliographyP latpar)
                --     then [""]
                --     else
                makebiblio 
                    
                            -- (fromJustNote "tex2latex 2wrqwe" $ latStyle latpar)
                            -- (case (latBibliographyP  latpar) of 
                            --         Nothing -> error "tex2latex dwerdd00"
                            --         Just a -> s2t . toFilePath $ a 
                            -- )
          , unlines' postamble1
          ]

--   where 
--     latpar2 = latpar{
--             latBibliographyP = maybe ""  (s2t . toFilePath) $ latBibliographyP latpar,
--             latStyle = maybe "authoryear" id $ latStyle latpar
--             }
-- todo  - macche einen file

latexLangConversion :: Text -> Text 
latexLangConversion inlang = 
    case lang2 of 
        "de" -> "ngerman"
        "en" -> "english"
        _ -> "english"
    where 
        lang2 = take' 2 inlang

preamble1 ::   Path Abs Dir -> LatexParam -> [Text]
-- pass the webroot (baked site) to set for graphics path 
preamble1 webroot  latpar =
    [ -- "%%% eval: (setenv \"LANG\" \"de_CH.utf8\")",
    --   "\\documentclass[a4paper,10pt,notitlepage]{scrbook}"
      "\\documentclass[a4paper,10pt,notitlepage]{scrartcl}"
      -- notitlepage makes title and abstract and text on same page
    , "\\usepackage{fontspec}"
     -- "\\setsansfont{CMU Sans Serif}%{Arial}",  -- not for xetex
      -- "\\setmainfont{CMU Serif}%{Times New Roman}",
      -- "\\setmonofont{CMU Typewriter Text}%{Consolas}",
      -- only useful for book laguage (in index) 
    ,  "\\usepackage[" <> latLanguage latpar <> "]{babel}"
    , "\\renewcaptionname{ngerman}{\\bibname}{Literatur}   %Bibliography"
    , "\\renewcaptionname{english}{\\bibname}{References}   %Bibliography"
    , "\\usepackage{graphicx}"
    , "          \\setkeys{Gin}{width=.75\\linewidth,keepaspectratio}"
    -- set defaults for includegraphics, to make pictures not too big in pdf
    , "\\usepackage{makeidx}"
    -- , "\\usepackage{natbib}"
    , "\\usepackage[backend=biber," --  %% Hilfsprogramm "biber" (statt "biblatex" oder "bibtex")
    ,   "style=" <> latStyle latpar <> "," -- %% Zitierstil (siehe Dokumentation)
    ,   "natbib=true," --  %% Bereitstellen von natbib-kompatiblen Zitierkommandos
    ,   "hyperref=true," -- %% hyperref-Paket verwenden, um Links zu erstellen
    ,   "]{biblatex}"
    , "\\addbibresource{" <>  latBibliography latpar <> "}"
    , "\\addbibresource{jobname.bib}"

    -- , "\\addbibresource{/home/frank/Workspace11/ssg/docs/site/dough/resources/BibTexLatex.bib}"
    
    -- , "\\newenvironment{abstract}{}{}" -- is this necessary

    -- , "\\usepackage{abstract}" -- not necessary
    , "\\makeindex"
    , "\\usepackage[colorlinks]{hyperref}"
    , "\\usepackage{bookmark}"  -- to avoid the need for rerun lualatex, must be loaded after hyperref
    , "\\providecommand{\\tightlist}{%"
    , "\\setlength{\\itemsep}{0pt}\\setlength{\\parskip}{0pt}}"
    , ""
    ,	"\\title{"<> latTitle latpar <> "}"
    ,   "\\author{" <> latAuthor latpar <> "}"
    -- does this produce nothing if the author field is empty? TODO
    ,   "\\date{}"  -- no date
    , "\\graphicspath{{"<>(s2t $ toFilePath webroot) <> "}}"  -- to find image the same place than html web root
    , ""
    , "\\begin{document}"
    , ""
    , "\\usepackage{filecontents}"
    ,       "\\begin{filecontents}{\\jobname.bib}"
    ,       latReferences latpar
    ,       "\\end{filecontents}"
    , ""
    , "\\maketitle"
	, "\\begin{abstract}" <> latAbstract latpar <> "\\end{abstract}"
    , "\\bigskip" -- a blank line after the abstract
    , ""
    ] ::
        [Text]

postamble1 = ["", "", "\\printindex", "\\end{document}"] :: [Text]

makebiblio ::   [Text] 
makebiblio  =
    [ ""
    , ""
    -- , "\\bibliographystyle{plainnat}"
    , "\\printbibliography"
    -- , "\\bibliography{" <>  biblio <> "}"
    , ""
    ]  

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


