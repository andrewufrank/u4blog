
 -----------------------------------------------------------------------------
--
-- Module      :  files for Http 
--
-- | using http simple to sparql queries and to create requests
-- part of uniform (to use only text
-- wraps URI in URI

-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE IncoherentInstances      #-}  -- necessary for overlapping
-- {-# LANGUAGE OverlappingInstances #-} 
{-# LANGUAGE Unsafe #-} 
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric  #-}
-- {-# LANGUAGE StandaloneDeriving
-- --    , GeneralizedNewtypeDeriving
--     , DeriveGeneric
--     , DeriveAnyClass
--       #-}
 {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.HttpFiles (
        -- TimeOutSec, mkTimeOut, mkTimeOutDefault
        -- , URI, HttpQueryParams
    module Uniform.HttpFiles
    -- , module Uniform.Zero
    -- , module Uniform.FileIO
--    , module N.Network.URI
    -- , uriT
            )  where


-- import qualified Network.URI as N
-- -- import  Network.URI (URI(..)) 
-- -- URI is a newtype with URI as a wrapper
-- import           Uniform.Error (errorT)
-- import           Uniform.Json
-- import           Uniform.ListForm -- (IsString (..), (</>), (<.>))
-- import           Uniform.Strings 
import           UniformBase

import           Uniform.Json
--import qualified   Network.URI.Encode as N2

-------------------------------------------------------HTML files
extHTML :: Extension
extHTML = Extension "html"

newtype HTMLout = HTMLout {contentHtml :: Text}
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON HTMLout

-- a wrapper around html ready to publish
unHTMLout (HTMLout a) = a

htmloutFileType = TypedFile5{tpext5 = extHTML} :: TypedFile5 Text HTMLout

instance Zeros HTMLout where
    zero = HTMLout zero

instance TypedFiles7 Text HTMLout where
    wrap7 = HTMLout
    unwrap7 (HTMLout a) = a

-- extension in metapage