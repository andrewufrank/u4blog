---------------------------------------------------------------------------
--
-- Module      : reading bibtex and producing the list for nocite
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Uniform.BibTex (
    module Uniform.BibTex
    , Reference
    ) where -- (openMain, htf_thisModuelsTests)

import Data.List (intersperse)
import qualified Data.Map as M
-- import           System.Directory (setCurrentDirectory, getCurrentDirectory)

-- import Text.BibTeX.Entry ()
import Text.BibTeX.Entry as T ( T(fields, identifier) )
import qualified Text.BibTeX.Entry as Entry
-- import           Text.BibTeX.Parse
import qualified Text.BibTeX.Parse as Parse
import Text.CSL as Citeproc
    ( Reference, readBiblioFile, readCSLFile )  
import Text.CSL.Pandoc as Bib (processCites, processCites')
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Definition as PD
import qualified Text.Parsec as Parsec
-- import           Uniform.Error
-- import           Uniform.FileIO
import Uniform.PandocImports
    ( Pandoc, getMeta, putMeta, fromJSONValue )
import UniformBase
    
import Uniform.Json ( Value, Result, Value(Null) ) 

readBiblioRefs ::  
  Bool
  -> Path Abs File  -- ^ for biblio FilePath
  -> Maybe Text  -- ^ locale - not used in citeproc-hs
  -> Path Abs File -- ^ for csl FilePath   
  -> Maybe Value -- ^ references in the md file
  -> Pandoc
  -> ErrIO Pandoc
  -- unpack the references in the pandoc value
  -- call readBiblioFile from citeproc and put refs into metadata
  -- the file path should be absolute path 
readBiblioRefs debugx biblio1 loc1 style1  refs1 p1 = do 
    let bibliofp = toFilePath biblio1
    let stylefp = toFilePath style1
    when debugx $ putIOwords ["readBiblioRefs-3-1"
            , "bibliofp", s2t bibliofp]
    let refs2 = fromMaybe Null  refs1  :: Value
    let refs4 = fromMaybe [] $ fromJSONValue refs2  :: [Reference]
    -- let refs4 = fromJustNote "readBiblioRefs 08werwe refs4" refs3 :: [Reference]
    biblio2 <- callIO $ Citeproc.readBiblioFile (const True) bibliofp
    -- seems to be requiring a full path 
    -- (long discussion https://github.com/jgm/pandoc/issues/5982 )

    when debugx $ putIOwords ["readBiblioRefs-3-2", "done"]
    style2 <- callIO $ Citeproc.readCSLFile loc1 stylefp
    -- citeproc will not allow loc1 but add the locale later with mergeLocale or similar 
    -- error with language (de_at, but de or en works)
    when debugx $ putIOwords ["readBiblioRefs-3-3", "done"]

    let refsSum = refs4 ++ biblio2
    let p2 = processCites style2 refsSum p1
    when debugx $ putIOwords ["readBiblioRefs-3-4", "done"]

    return p2 


{-
, MetaBlocks
                   [ Plain
                       [ Cite
                           [ Citation
                               { citationId = "frank-machbarkeit"
                               , citationPrefix = []
                               , citationSuffix = []
                               , citationMode = AuthorInText
                               , citationNoteNum = 0
                               , citationHash = 0
                               }
                           ]
                           [ Str "@frank-machbarkeit" ]
                       , Space
                       , Cite
                           [ Citation
                               { citationId = "frank09geo"
                               , citationPrefix = []
                               , citationSuffix = []
                               , citationMode = AuthorInText
                               , citationNoteNum = 0
                               , citationHash = 0
                               }
                           ]
                           [ Str "@frank09geo" ]
                       , Space

                       , Cite
                           [ Citation
                               { citationId = "Frank2010a"
                               , citationPrefix = []
                               , citationSuffix = [ Str "TUxx9999" ]
                               , citationMode = AuthorInText
                               , citationNoteNum = 0
                               , citationHash = 0
                               }
                           ]
                           [ Str "@Frank2010a" , Space , Str "[TUxx9999]" ]
                       ]
                   ]
               )
-}

fillCitation :: Text -> PD.Citation
-- ^ fill a citation with an id for the citation (nothing else)
fillCitation citID =
  PD.Citation
    { PD.citationId = citID,
      PD.citationPrefix = zero,
      PD.citationSuffix = zero,
      PD.citationMode = PD.AuthorInText,
      PD.citationNoteNum = zero,
      PD.citationHash = zero
    }

constructNoCite :: [Text] -> P.Meta
-- ^ construct the structure for the nocite info
constructNoCite bibids = P.Meta map1
  where
    cits = map (\s -> [fillCitation s]) bibids :: [[PD.Citation]]
    refs = map (\s -> [PD.Str ("@" <> s)]) bibids :: [[PD.Inline]]
    cites = zipWith PD.Cite cits refs :: [PD.Inline]
    metablocks = PD.MetaBlocks [PD.Plain (intersperse PD.Space cites)]
    map1 = M.insert "nocite" metablocks M.empty

pandocProcessCites ::
  Path Abs Dir -> Path Abs File -> Maybe Text -> Pandoc -> ErrIO Pandoc
-- ^ process the citations
-- including the filling the references for publication lists
pandocProcessCites doughP biblio groupname pandoc1 = do
  pandoc2 <- case groupname of
    Nothing -> return pandoc1
    Just gn -> do
      bibids <- bibIdentifierFromBibTex biblio (t2s gn)
      let meta1 = constructNoCite (map s2t bibids)
      --                    let cits = map (\s -> [fillCitation . s2t $ s]) bibids :: [[PD.Citation]]
      --                    let refs = map (\s -> [PD.Str ("@" <> s)]) bibids :: [[PD.Inline]]
      --                    let cites = zipWith PD.Cite cits refs  :: [PD.Inline]
      --                    let metablocks = PD.MetaBlocks [PD.Plain cites]
      --                    let map1 = M.insert "nocite" metablocks M.empty
      let meta2 = getMeta pandoc1 :: P.Meta
      --                    let meta3 = meta2 <> P.Meta map1
      let pandoc2 = putMeta (meta2 <> meta1) pandoc1
      return pandoc2
  --    callIO $ do
  currDir <- currentDir
  -- the current dir is the directory in which the procCites of pando will
  -- search.

  --            putIOwords ["markdownToPandoc", "currDir", showT currDir, "\ndoughP", showT doughP]
  --            putIOwords ["markdownToPandoc", "bibfp", showT bib]
  setCurrentDir doughP
  res <- callIO $ processCites' pandoc2
  setCurrentDir currDir
  --            putIOwords ["markdownToPandoc", "again currDir", showT currDir, "\nwas doughP", showT doughP]
  return res

readBibTex :: FilePath -> IO String
-- ^ reads the bibtex file
readBibTex fp = do
  bib <- readFile fp
  --    putIOwords ["readBibTex", showT bib]
  return bib

parseBibTex :: String -> IO [Entry.T]
parseBibTex bib =
  case Parsec.parse (Parse.skippingLeadingSpace Parse.file) "stdin" bib of
    --  parseBibTex bib = case Parsec.parse (Parsec.skipMany Parsec.space >> Parse.file) "stdin" bib of
    Left errMsg -> error (show errMsg)
    Right entries -> return entries

filterByGroup ::
  String ->
  [Entry.T] ->
  -- | select the entries in a group
  [Entry.T]
filterByGroup groupname = filter (elem groupname . getGroup)

getGroup :: Entry.T -> [String]
getGroup e = map snd groupFields
  where
    fs = fields e :: [(String, String)]
    groupFields = filter (("groups" ==) . fst) fs

getBibIdentifier :: [Entry.T] -> [String]
-- ^ extract the bib identifier from the entries
-- getBibIdentifier es = map T.entryType es
getBibIdentifier = map T.identifier

bibIdentifierFromBibTex :: Path Abs File -> String -> ErrIO [String]
-- ^ combine the process
bibIdentifierFromBibTex bibfn group = callIO $ do
  bib <- readBibTex (toFilePath bibfn)
  entries <- parseBibTex bib
  let es = filterByGroup group entries
      ids = getBibIdentifier es
  return ids
