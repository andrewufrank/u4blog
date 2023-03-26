-------------------------------------------------------------------
--
-- Module      :   start the code to produce a pdf 
----------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- | the main for the example
     
-}
module Uniform.Pandoc2pdf 
    (module Uniform.Pandoc2pdf
    ) where  

import UniformBase  

import qualified Text.Pandoc.PDF as Pan (makePDF)
import qualified Text.Pandoc as Pan
import qualified Data.ByteString.Lazy as BL
-- import Text.Pandoc (Extension)

-- import FileTypes ( Panrep(panpan), panrepFileType, pdfFileType ) 
-- import MetaPage (extPDF)


panrep2pdf  :: NoticeLevel -> Pan.Pandoc -> Path Abs File -> Extension ->  ErrIO ()
-- | convert the panrefp to pdf and write pdf file
-- attention: this is ErrIO wrapping callIO wrapping (pandoc) runIO
-- return from runIO does something with error msg (and types)
-- note: the pdf is written from the lazybytestream directly
-- not using write8 

panrep2pdf debug dr1 resFn ext = do 
    -- stuff done with write8 automatically
    let parent = getParentDir resFn
    createDirIfMissing' parent
    let resFn2 = resFn <.> ext

    res9 <- callIO $ do 
        res0 <- Pan.runIO $ do 

        -- run makePDF 
            t2 <- Pan.compileDefaultTemplate "latex"
            Pan.makePDF "lualatex" [] 
                    Pan.writeLaTeX 
                    (Pan.def {  Pan.writerTemplate = Just t2}) 
                    (dr1)
        -- -- before error handling 
        --     -- liftIO $ putStrLn "\nres1"
        --     -- liftIO $ putStrLn (show res1)  -- Right contains something long and complex
        --             -- starting with %PDF-1.5
            
        --     -- case separation must be inside runIO 
        --     -- case res1 of    
        --     --     Right b -> liftIO $ BL.writeFile (toFilePath resFn2) b 
        --     --         --  callIO $ do 
        --     --     --                         write8 resFn pdfFileType  (PDFfile b)
        --     --     --                         return b 
        --     --                     -- to complicated to convert lazy bytestring 
                                
        --     --     Left  x -> errorT ["error in pandoc conversion to pdf", bb2t . bl2b $ x] 
        --     --     -- liftIO $ putStrLn "Export error"
            
        --     return res1
        -- res3 :: Either BL.ByteString BL.ByteString <- handleError res0 
        Pan.handleError res0 
        

    -- putIOwords ["?", showT res9]
    case res9 of 
            Right b -> liftIO $ BL.writeFile (toFilePath resFn2) b 
            -- putIOwords ["ok return from pandoc conversion to pdf", "file already written"] 
            Left msg -> throwErrorWords ["error in pandoc conversion to pdf", bl2t msg]
    return ()

