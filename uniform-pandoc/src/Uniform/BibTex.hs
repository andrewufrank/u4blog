---------------------------------------------------------------------------
--
-- Module      : process the references in the pandoc data

-- was : reading bibtex and producing the list for nocite
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Uniform.BibTex (
    module Uniform.BibTex
    ) where 

import qualified Text.Pandoc.Citeproc as PC

import Uniform.PandocImports
--     ( Pandoc, getMeta, putMeta, fromJSONValue )
import UniformBase
    

pandocProcessCites ::
--   Path Abs Dir -> Path Abs File -> Maybe Text ->
       Pandoc -> ErrIO Pandoc
-- ^ process the citations
-- including the filling the references for publication lists
-- not done currently? need example
pandocProcessCites   pandoc1 = do

    pandoc9 <- unPandocM $ PC.processCitations pandoc1
    return pandoc9



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

