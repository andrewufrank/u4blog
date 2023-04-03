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

module Uniform.WritePDF
  ( module Uniform.WritePDF
  ) where

import qualified System.Exit as Sys
import qualified System.Process as SysP
import System.IO.Silently (silence)
-- import Uniform.Json
import UniformBase




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
    when (inform debug) $ putIOwords
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
        SysP.withCreateProcess -- "callProcess"
            (SysP.proc cmd   args)
                { SysP.delegate_ctlc = True
                , SysP.cwd = Just . toFilePath $ cwd1
                }
            $ \_ _ _ p ->
                SysP.waitForProcess p
    return exit_code 

