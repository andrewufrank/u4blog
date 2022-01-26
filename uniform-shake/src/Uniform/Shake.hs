{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# LANGUAGE UndecidableInstances  #-}

module Uniform.Shake ( 
          module Uniform.Shake
        , module Uniform.Shake.Path
        , takeBaseName, splitPath 
        , Action
        , module UniformBase
        , Rules
        , shakeArgs, shake, ShakeOptions(..), shakeOptions
        , Verbosity(..), Lint(..)
        , need, (%>),  (|%>) 
        , want, phony
        )      where

import Development.Shake hiding (Error )
        -- (Action, FilePattern, getDirectoryFiles, copyFileChanged)
import Development.Shake.FilePath (takeBaseName, splitPath
                        )
     
import UniformBase
import Control.Exception (throw)  -- to deal with errors in action
import Uniform.Shake.Path

($-<.>) :: Path a File -> Text ->  Path a File
f $-<.> e = replaceExtension' e f 

($--<.>) :: Path a File -> Text ->  Path a File
f $--<.> e = replaceExtension2 e f 

replaceExtension' :: Text -> Path a File -> Path a File
-- a flipped version of -<.> 
replaceExtension' newext  =
    setExtension (makeExtension . t2s $ newext) 
replaceExtension2 :: Text -> Path a File -> Path a File
-- remove a doubled extension (e.g. gutenberg.txt)
replaceExtension2 newext  =
    setExtension (makeExtension . t2s $ newext) . removeExtension

    -- if isRelative filen 
    --     then makeRelFile resn 
    --     else makeAbsFile resn
    --     where 
    --             filen = toFilePath filep 
    --             resn = replaceExtension (t2s newext) filen 

getDirectoryFilesP :: Path Abs Dir -> [FilePattern] -> Action [Path Rel File]
getDirectoryFilesP d p = do
    res :: [FilePath] <- getDirectoryFiles (toFilePath d) p
    return $ map makeRelFile res

copyFileChangedP :: Path Abs File -> Path Abs File -> Action ()
copyFileChangedP infile outf = copyFileChanged (toFilePath infile) (toFilePath outf)

class Path2nd  a c where
    stripProperPrefixP :: Path a b -> Path a c -> Path Rel c
    makeRelativeP  :: Path a Dir -> Path a c -> Path Rel c
    makeRelativeP = stripProperPrefixP
    -- ^ strip the first (the prefix) from the second and returns remainder 
    -- throws error when not prefix or not proper file path 
    replaceDirectoryP :: Path a Dir -> Path a Dir -> Path a c  -> Path a c
    -- ^ strip the first (the prefix) and add the second to the third 
    
instance   Path2nd  a File where
    stripProperPrefixP a b = fromJustNote
        ( t2s
        . unwords'
        $ ["Path2nd Dir - not a prefix", s2t . toFilePath $  a, "for",  s2t . toFilePath $ b]
        )
        (fmap makeRelFile ab)
        where ab = stripPrefix' (toFilePath a) (toFilePath b) :: Maybe FilePath

    replaceDirectoryP pref newpref old = newpref </> rem1 
        where rem1 = stripProperPrefixP pref old


instance Path2nd  a Dir where
    stripProperPrefixP a b = fromJustNote
        ( t2s
        . unwords'
        $ ["Path2nd Dir - not a prefix",  s2t . toFilePath $ a, "for",  s2t . toFilePath $ b]
        )
        (fmap makeRelDir ab)
        where ab = stripPrefix' (toFilePath a) (toFilePath b) :: Maybe FilePath

    replaceDirectoryP pref newpref old = newpref </> rem1 
        where rem1 = stripProperPrefixP pref old

runErr2action :: ErrIO a -> Action a
runErr2action op = liftIO $ do
    res <- runErr  op
    case res of
        Left msg -> fail . t2s . unwords' $ ["runErr2action", msg]
        Right a -> return a

-- throwAction :: Text -> Action () 
-- throwAction msg = liftIO . throwIO $ msg

getFilesToBake :: Text -> Path Abs Dir -> [FilePattern] 
        -> Action [Path Rel File]
-- | get all files according to the FilePattern (see Shake docs)
-- in the given directory
-- but excludes all filepath which contain one of the strings in 
-- the first argument to allow directories which are not baked

getFilesToBake exclude d p = do
    res :: [Path Rel File] <- getDirectoryFilesP d p
    let filtered = filter (not . (isInfixOf' exclude) . s2t .toFilePath ) res
    -- putIOwords [unlines' $ map (s2t . toFilePath) filtered]
    return   filtered
