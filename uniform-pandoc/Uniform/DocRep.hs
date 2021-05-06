---------------------------------------------------------------------------
--
-- Module      :  Uniform.DocRep
-- the abstract representation of the documents
-- consists of pandoc for text and
--              metajson for all other values
-- the text content is not in the metajson
-- (but can be put into the json )
-- metajson is just a wrapped json
-- DocRep replaces DocVal (Value is in json used)
-- metajson replaces metarec
-- DocRep can be read8/write8
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-unused-matches #-}

module Uniform.DocRep
  ( module Uniform.DocRep,
    HTMLout,
    htmloutFileType,
    -- , Dtemplate
    -- , Template
    -- , renderTemplate
  )
where

import Control.Lens -- needed for the query expressions
  ( (^?),
  -- , (?~)
  -- , (&)
  -- , at
  )
import Data.Aeson.Lens (key)
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    ToJSON,
    Value,
    parseMaybe,
  )
import GHC.Generics (Generic)
import Text.CSL as Pars (Reference, readBiblioFile, readCSLFile)
import Text.CSL.Pandoc as Bib (processCites)
import qualified Text.Pandoc as Pandoc
import Uniform.HTMLout
  ( HTMLout (HTMLout),
    html5Options,
    htmloutFileType,
    writeHtml5String,
  )
import Uniform.Json
  ( AtKey (getAtKey, putAtKey),
    ErrIO,
    FromJSON (parseJSON),
    ToJSON,
    Value,
    mergeRightPref,
  )
import Uniform.PandocImports (Pandoc, Panrep (Panrep), unPandocM)
import UniformBase

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

-- | a more uniform method to represent a document
-- the yam part contains the json formated yaml metadata
-- which is extensible
-- Attention the Pandoc is Pandoc (Meta (Map Text MetaValue) [Block]
-- means that title etc is duplicated in the Meta part.
-- it would be better to use only the block and all
-- metadata keept in the yam json
-- TODO replace Pandoc with Block in DocRep
data DocRep = DocRep {yam :: Value, pan :: Pandoc} -- a json value
  deriving (Show, Read, Eq, Generic)

instance Zeros DocRep where zero = DocRep zero zero

instance FromJSON DocRep

instance ToJSON DocRep

docRep2panrep :: DocRep -> ErrIO Panrep
-- ^ transform a docrep to a panrep (which is the pandoc rep)
-- does process the references
-- and will do index, but this goes to ssg
docRep2panrep dr1@(DocRep y1 p1) = do
  (DocRep y2 p2) <- addRefs dr1
  return $ Panrep y2 p2

------------------------------------

panrep2html :: Panrep -> ErrIO HTMLout
-- ^ transform a docrep to a html file
-- needs teh processing of the references with citeproc
panrep2html pr1@(Panrep y1 p1) = do
  -- dr2 <- addRefs pr1
  h1 <- unPandocM $ writeHtml5String html5Options (p1)
  return . HTMLout $ h1

--------------------------------
addRefs :: DocRep -> ErrIO DocRep
-- ^ add the references to the pandoc block
-- the biblio is in the yam (otherwise nothing is done)
-- ths cls file must be in the yam

-- processCites :: Style -> [Reference] -> Pandoc -> Pandoc

-- Process a Pandoc document by adding citations formatted according to a CSL style. Add a bibliography (if one is called for) at the end of the document.
-- http://hackage.haskell.org/package/citeproc-hs-0.3.10/docs/Text-CSL.html
--   m <- readBiblioFile "mybibdb.bib"
--   s <- readCSLFile "apa-x.csl"
--   let result = citeproc procOpts s m $ [cites]
--   putStrLn . unlines . map (renderPlainStrict) . citations $ result

addRefs dr1@(DocRep y1 p1) = do
  -- the biblio entry is the signal that refs need to be processed
  -- only refs do not work
  putIOwords ["addRefs", showT dr1, "\n"]
  let biblio1 = getAtKey y1 "bibliography" :: Maybe Text
  maybe (return dr1) (addRefs2 dr1) biblio1

addRefs2 :: (MonadIO m, MonadError m, ErrorType m ~ Text) 
    => DocRep -> Text -> m DocRep
addRefs2 dr1@(DocRep y1 p1) biblio1 = do
  when True $ putIOwords ["addRefs2-1", showT dr1, "\n"]
  let style1 = getAtKey y1 "style" :: Maybe Text
      refs1 = y1 ^? key "references" :: Maybe Value -- is an array
      nocite1 = getAtKey y1 "nocite" :: Maybe Text

  when True $
    putIOwords
      [ "addRefs2-2",
        "\n biblio",
        showT biblio1, -- is only biblio "resources/BibTexLatex.bib"
        "\n style",
        showT style1, -- style Just "/home/frank/Workspace8/ssg/docs/site/dough/resources/chicago-fullnote-bibliography-bb.csl"
        "\n refs",
        showT refs1,
        "\n nocite",
        showT nocite1
      ]

  let loc1 = (Just "de") -- TODO depends on language
  -- creates error later...
  let refs2 = fromJustNote "refs in addRefs2 vcbnf refs2" $ refs1 :: Value
  let refs3 = fromJSONValue $ refs2 -- :: Result [Reference]
  let refs4 = fromJustNote "addRefs2 08werwe refs4" refs3 :: [Reference]

  let bibliofp =
        t2s biblio1 :: FilePath
  let stylefp =
        t2s . fromJustNote "style1 in addRefs2 wer23" $ style1 :: FilePath
  --  Raised the exception:
  -- ["runErr2action","Safe.fromJustNote Nothing, style1 in docRepAddRefs wer23\nCallStack (from HasCallStack):\n  fromJustNote, called at ./Uniform/DocRep.hs:165:19 in uniform-pandoc-0.0.2-CQ6TrBvcdAe7Crud3c6Rca:Uniform.DocRep"]
  when True $ putIOwords ["addRefs2-3-1", "done"]

  biblio2 <- callIO $ Pars.readBiblioFile (const True) bibliofp
  when True $ putIOwords ["addRefs2-3-2", "done"]
  style2 <- callIO $ Pars.readCSLFile loc1 stylefp
        -- error with language (de_at)
  when True $ putIOwords ["addRefs2-3-3", "done"]

  let refsSum = refs4 ++ biblio2
  let p2 = processCites style2 refsSum p1

  when True $ putIOwords ["addRefs2-4", "p2\n", showT p2]

  return (DocRep y1 p2)

--------------------------------------------typed file DocRep

extDocRep :: Extension
extDocRep = Extension "docrep"

-- instance NiceStrings DocRep where
--   shownice = showNice . unDocRep

docRepFileType :: TypedFile5 Text DocRep
docRepFileType =
  TypedFile5 {tpext5 = extDocRep} :: TypedFile5 Text DocRep

instance TypedFiles7 Text DocRep where
  wrap7 = readNote "DocRep wrap7 sfasdwe" . t2s
  unwrap7 = showT

mergeAll :: DocRep -> [Value] -> DocRep
-- ^ merge the values with the values in DocRec -- last winns
-- issue how to collect all css?
mergeAll (DocRep y p) vs = DocRep (mergeRightPref $ y : vs) p

instance AtKey DocRep Text where
  getAtKey dr k2 = getAtKey (yam dr) k2

  putAtKey k2 txt (DocRep y p) = DocRep (putAtKey k2 txt y) p

-- instance AtKey DocRep Bool where
--   getAtKey dr k2 = getAtKey (yam dr) k2

--   putAtKey k2 b dr = DocRep $ putAtKey k2 b (unDocRep meta2)
