-----------------------------------------------------------------------------
--
-- Module      :  main for watch 
-----------------------------------------------------------------------------
 

    {-# LANGUAGE
            MultiParamTypeClasses
    -- , TypeSynonymInstances
--    , FunctionalDependencies
    -- , FlexibleInstances
    -- , FlexibleContexts
    -- , ScopedTypeVariables
--    , UndecidableInstances
        , OverloadedStrings
        -- , TypeFamilies

    #-}

module WatchMain     where



-- import   Uniform.Error
-- import Uniform.Strings
-- import Uniform.FileIO 
-- import Uniform.Watch_test
import Uniform.Watch
-- import Uniform.Convenience.StartApp

main = startProg
    "WatchMain"
    "testing watch"
            (do
                watchMain [testWatch] foreverScotty
            )

