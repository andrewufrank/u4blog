--------------------------------------------------------------------------
--
-- Module      :  Uniform.Shake
        -- top import, darf nicht von andern importiert werden hier 
-------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            #-}

module Uniform.Shake
  ( module Development.Shake 

  )
where

import Development.Shake
  ( shake,
        shakeOptions,
        (%>),
        phony,
        want,
        Action,
        Lint(LintBasic),
        ShakeOptions(shakeFiles, shakeVerbosity, shakeLint),
        Rules,
        (|%>),
        Verbosity(..) )
