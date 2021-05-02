-----------------------------------------------------------------------------
--Main.hs
-- Module      :  Main
-- Copyright   :  andrew u frank 2016
--
-- | test  for http
-- empty so far
--
-----------------------------------------------------------------------------
{-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
--    , FlexibleContexts
--    , DeriveFunctor
    , ScopedTypeVariables
--    , UndecidableInstances
    , OverloadedStrings
    #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Main (main) where


--import qualified Data.Text as T (Text)
-- import Data.Strings
import System.Exit
-- import TestingFileIO
import UniformBase

programName = "uniform-http test"

debug_main  =  True


main :: IO ()
main = do
    putIOwordsT ["start  \n" ]
    putIOwordsT [ "------------------ ", programName
--                , toText versionNum
            , " -------------------------"]
    -- r1 <- fileioTest
    let r1 = [True]
    -- putIOwords["main", programName, "returning\n"
    --         , unwords' . map show' $ r1
    --         , "-------------------------\n\n"]
    let bs = r1
    putIOwords ["Test Main", showT bs]
    if (and bs) then exitSuccess else exitFailure



