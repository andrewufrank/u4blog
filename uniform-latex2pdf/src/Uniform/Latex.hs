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

data LatexParam = LatexParam
    { latBibliography :: Maybe Text
    , latStyle :: Maybe Text
    }
    deriving (Eq, Ord, Read, Show, Generic)

instance Zeros LatexParam where zero = LatexParam zero zero

doclatexOptions =
    defaultOptions
        { fieldLabelModifier = t2s . toLowerStart . s2t . drop 2
        }

instance ToJSON LatexParam where
    toJSON = genericToJSON doclatexOptions

instance FromJSON LatexParam where
    parseJSON = genericParseJSON doclatexOptions

-- TODO create a default/minimal preamble 
-- and add preamble as parameter

tex2latex :: LatexParam -> [Text] -> Text
{- ^ combine a snipped (produced from an md file) with a preamble to
 produce a compilable latex file.
 references are processed earlier (in  panrep)
-}
tex2latex latpar snips = concat'
        $ [ unlines' preamble1
          , concat' snips -- (map unTexSnip snips)
          , unlines' $
                if isZero latpar
                    then [""]
                    else
                        makebiblio
                            (fromJustNote "tex2latex 2wrqwe" $ latStyle latpar)
                            (fromJustNote "tex2latex 00wr" $ latBibliography latpar)
          , unlines' postamble1
          ]

-- todo  - macche einen file

preamble1 :: [Text]
preamble1 =
    [ -- "%%% eval: (setenv \"LANG\" \"de_CH.utf8\")",
      "\\documentclass[a4paper,10pt]{scrbook}"
    , "\\usepackage{fontspec}"
    , -- "\\setsansfont{CMU Sans Serif}%{Arial}",  -- not for xetex
      -- "\\setmainfont{CMU Serif}%{Times New Roman}",
      -- "\\setmonofont{CMU Typewriter Text}%{Consolas}",
      "\\usepackage[ngerman]{babel}"
    , "\\usepackage{graphicx}"
    , "\\usepackage{makeidx}"
    , "\\usepackage{natbib}"
    , "\\makeindex"
    , "\\usepackage[colorlinks]{hyperref}"
    , "\\providecommand{\\tightlist}{%"
    , "\\setlength{\\itemsep}{0pt}\\setlength{\\parskip}{0pt}}"
    , ""
    , "\\begin{document}"
    , ""
    ] ::
        [Text]

postamble1 = ["", "", "\\printindex", "\\end{document}"] :: [Text]

makebiblio style biblio =
    [ ""
    , ""
    , "\\bibliographystyle{plainnat}"
    , ""
    , "\\bibliography{" <> biblio <> "}"
    , ""
    ]

writePDF2 :: NoticeLevel -> Path Abs File -> Path Abs File -> Path Abs Dir -> ErrIO ()
-- convert the text in the file given (a full latex, exetnsion "tex") into a pdf
-- in the second path
-- set the current working directory (which must be the directory
-- from which images etc. are searched for )
writePDF2 debug fn fnres refDir = do
    -- -- check for locale
    -- loc <- callIO $ Sys.callProcess "locale" []
    -- putIOwords ["writePDF2text locale "]
    -- ls <- callIO $ Sys.callProcess "ls" []
    -- putIOwords ["writePDF2text ls "]

    -- process

    let infn = fn -- setExtension extTex fn :: Path Abs File
    when (inform debug) $ when (inform debug) $ putIOwords
        [ "writePDF2text 1 infn"
        , showT infn
        , "\n\t fnres"
        , showT fnres
        , "\n\t refDir"
        , showT refDir
        ]
    let dir1 = getParentDir fnres :: FilePath
    let out1 = "--output-directory=" <> dir1
    when (inform debug) $ putIOwords ["writePDF2text 2 out1", showT out1]
    callProcessWithCWD
        "lualatex"
        [out1, "-interaction=nonstopmode", toFilePath infn]
        refDir
    when (inform debug) $ putIOwords ["writePDF2text end"]
    -- callIO $ Sys.callProcess "xelatex" [out1,  "-interaction=nonstopmode" , toFilePath infn]
    -- callIO $ Sys.callProcess "lualatex" [out1, toFilePath infn]

    -- does not work to read pdf.
    -- the files written seem ok
    -- let resfn = setExtension extPDF  fn
    -- putIOwords ["writePDF2text 4 pdf filename", showT resfn]

    -- resPDFtext :: pdfFileType <- read8 resfn pdfFileType
    -- putIOwords ["writePDF2text lualatex result ok (otherwise error)"
    --             , "pdf is", take' 300 . unwrap7 $ resPDFtext]

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
callProcessWithCWD :: FilePath -> [String] -> Path Abs Dir -> ErrIO ()
callProcessWithCWD cmd args cwd1 = callIO . silence $ do
    exit_code <-
        Sys.withCreateProcess -- "callProcess"
            (Sys.proc cmd args)
                { Sys.delegate_ctlc = True
                , Sys.cwd = Just . toFilePath $ cwd1
                }
            $ \_ _ _ p ->
                Sys.waitForProcess p
    case exit_code of
        Sys.ExitSuccess -> return ()
        Sys.ExitFailure r -> fail . show $ r

-- processFailedException :: String -> String -> [String] -> Int -> IO a
-- processFailedException fun cmd args exit_code =
--       ioError (mkIOError OtherError (fun ++ ": " ++ cmd ++
--                                      concatMap ((' ':) . show) args ++
--                                      " (exit " ++ show exit_code ++ ")")
--                                  Nothing Nothing)

-- -- wrapper so we can get exceptions with the appropriate function name.
-- withCreateProcess_
--   :: String
--   -> CreateProcess
--   -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
--   -> IO a
-- withCreateProcess_ fun c action =
--     C.bracketOnError (createProcess_ fun c) cleanupProcess
--                      (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)

-- withCreateProcess
--   :: CreateProcess
--   -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
--   -> IO a
-- withCreateProcess c action =
--     C.bracket (createProcess c) cleanupProcess
--               (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)