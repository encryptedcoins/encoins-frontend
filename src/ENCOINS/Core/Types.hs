{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module ENCOINS.Core.Types where

import           Data.ByteString
import           Prelude          

-- TODO: this must follow the code in our encoins-core repository


type GroupElement = ByteString

type FieldElement = ByteString

-- TODO: implement this
{-# INLINABLE toFieldElement #-}
toFieldElement :: Integer -> FieldElement
toFieldElement = undefined

-- TODO: implement this
{-# INLINABLE fromFieldElement #-}
fromFieldElement :: FieldElement -> Integer
fromFieldElement = undefined

data MintingPolarity = Mint | Burn
    deriving (Eq, Show)

{-# INLINABLE polarityToInteger #-}
polarityToInteger :: MintingPolarity -> Integer
polarityToInteger Mint = 1
polarityToInteger Burn = -1