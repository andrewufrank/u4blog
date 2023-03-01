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
  ) where

import qualified System.Exit as Sys
import qualified System.Process as Sys
import System.IO.Silently (silence)
import Uniform.Json
import UniformBase

-- import Data.Aeson
-- import Data.Aeson.Types


data LatexParam = LatexParam
-- | the fields from the yaml date passed to latex-pdf
    { latTitle ::  Text  
    , latAuthor :: Text 
    , latAbstract ::  Text
    , latBibliographyP :: Text  -- the bibliio file 
            -- problem with multiple files? 
    , latStyle :: Text
            -- is not used 
    , latReferences :: Text  -- ^ used only for citeproc to produce html, not for biblatex to produce pdf 
    , latBook :: Bool  -- is this a long text for a book/booklet
    , latContent :: [Text] -- ^ a list of the .md files which are collected into a multi-md pdf
    }
    deriving (Eq, Ord, Read, Show, Generic)

instance Zeros LatexParam where 
    zero = LatexParam zero zero zero zero zero zero zero zero

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

tex2latex :: LatexParam ->  Text -> Text
{- ^ combine a snipped (produced from an md file) with a preamble to
 produce a compilable latex file.
 references are processed earlier (in  panrep)
 keps the metadata
-}
tex2latex latpar snips = concat'
        $ [ unlines' 
                (preamble1 
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

preamble1 ::   LatexParam -> [Text]
preamble1   latpar =
    [ -- "%%% eval: (setenv \"LANG\" \"de_CH.utf8\")",
    --   "\\documentclass[a4paper,10pt,notitlepage]{scrbook}"
      "\\documentclass[a4paper,10pt,notitlepage]{scrartcl}"
      -- notitlepage makes title and abstract and text on same page
    , "\\usepackage{fontspec}"
    , -- "\\setsansfont{CMU Sans Serif}%{Arial}",  -- not for xetex
      -- "\\setmainfont{CMU Serif}%{Times New Roman}",
      -- "\\setmonofont{CMU Typewriter Text}%{Consolas}",
      "\\usepackage[ngerman]{babel}"
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
    , "\\addbibresource{" <>  latBibliographyP latpar <> "}"
                    -- <> ", jobname.bib
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
    , "\\graphicspath{{/home/frank/bakedHomepage}}"  -- to find image the same place than html web root
    , ""
    , "\\begin{document}"
    , ""
    -- , "\\usepackage{filecontents}"
    -- ,       "\\begin{filecontents}{\\jobname.bib}"
    -- ,       latReferences latpar
    -- ,       "\\end{filecontents}"
    -- , ""
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

writePDF2 :: NoticeLevel -> Path Abs File -> Path Abs File -> Path Abs Dir -> ErrIO ()
-- convert the text in the file given (a full latex, exetnsion "tex") into a pdf
-- in the second path
-- refDir is the current working directory (which must be the directory
-- where the intermediate files are produced
--  likely wrong: from which images etc. are searched for )
writePDF2 debug fn fnres refDir = do
    -- -- check for locale
    -- loc <- callIO $ Sys.callProcess "locale" []
    -- putIOwords ["writePDF2 locale "]
    -- ls <- callIO $ Sys.callProcess "ls" []
    -- putIOwords ["writePDF2 ls "]

    -- process

    let infn =   getNakedFileName $ fn :: FilePath -- setExtension extTex fn :: Path Abs File
    when (informAll debug) $ when (inform debug) $ putIOwords
        [ "writePDF2 1 infn"
        , showT infn
        , "\n\t fnres"
        , showT fnres
        , "\n\t refDir (will be current working dir but seem not to work)"
        , showT refDir
        ]
    let dir1 = getParentDir fnres :: FilePath 
    let out1 = "--output-directory=" <> dir1
    when (inform debug) $ putIOwords ["writePDF2 2 out1", showT out1]

    exit_code1 <- callProcessWithCWD True -- (not (inform debug))  -- silenced or not 
        "lualatex"
        [out1, "-interaction=nonstopmode",  infn]
        refDir
    exitHandling exit_code1 infn 1

    exit_code2 <- callProcessWithCWD True -- (not (inform debug))  -- silenced or not 
        "biber"
        [ infn]
        refDir
    exitHandling exit_code2 infn 2

    exit_code3 <- callProcessWithCWD True -- (not (inform debug))  -- silenced or not 
        "makeindex"
        ["-q", infn]
        refDir
    exitHandling exit_code3 infn 3

    exit_code3 <- callProcessWithCWD True -- (not (inform debug))  -- silenced or not 
        "lualatex"
        [out1, "-interaction=nonstopmode",  infn]
        refDir
    exitHandling exit_code3 infn 4

    when (inform debug) $ putIOwords ["writePDF2 end for", showT out1]

exitHandling :: Sys.ExitCode -> FilePath -> Int -> ErrIO ()
exitHandling exit_code filename step = do
    -- the count indicated the step count 
    case exit_code of
        Sys.ExitSuccess -> return ()
        Sys.ExitFailure r -> do 
                putIOwords ["callProcessWithCWD - failed - check for 1 log, for 2 blg " 
                            , "show exit code", showT r, "step", showT step
                            -- , "\n\tif lualatex: 1 is normal, check log file "
                            -- , "\n\tif biber: 2 is normal, check blg file "
                            -- , "\n\tfor output file", showT filename
                            ]
                -- fail . show $ r
                return ()  -- lualatex does not deal with error information well - check log file 


    return ()

--------------------------------
-- callProcess/callCommand with current working directory
-- from http://hackage.haskell.org/package/process-1.6.10.0/docs/src/System.Process.html#callProcess

{- | Creates a new process to run the specified command with the given
 arguments, and wait for it to finish.  If the command returns a non-zero
 exit code, an exception is raised.

 If an asynchronous exception is thrown to the thread executing
 @callProcess@, the forked process will be terminated and
 @callProcess@ will wait (block) until the process has been
 terminated.

 @since 1.2.0.0

 wrapped in silence to avoid output on std out
-}
callProcessWithCWD :: Bool ->  FilePath -> [String] -> Path Abs Dir -> ErrIO Sys.ExitCode
-- | call a process silenced 
-- cwd1 is the curren working dir (where the intermediate files are
-- but seems not to be the place where biblio is searched for
callProcessWithCWD silenced cmd args cwd1 = callIO 
        . (if silenced then (silence) else (id)) $ do -- . silence $ do
    exit_code <-
        Sys.withCreateProcess -- "callProcess"
            (Sys.proc cmd   args)
                { Sys.delegate_ctlc = True
                , Sys.cwd = Just . toFilePath $ cwd1
                }
            $ \_ _ _ p ->
                Sys.waitForProcess p
    return exit_code 

