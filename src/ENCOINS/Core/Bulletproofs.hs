{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module ENCOINS.Core.Bulletproofs where

import           Prelude               (Bool (..), Integer)

import           ENCOINS.Core.Types    (GroupElement, MintingPolarity (..))

-- TODO: this must follow the code in our encoins-core repository


type Input = ([(GroupElement, MintingPolarity)], Integer)

-- TODO: implement this
type Proof = ()

fakeProof :: Proof
fakeProof = ()

-- TODO: implement this
{-# INLINABLE verify #-}
verify :: Input -> Proof -> Bool
verify _ _ = True