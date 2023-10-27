{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Backend.Protocol.Fees (protocolFees) where

import           PlutusTx.Prelude       hiding (mapM, (<$>), (<>))

import           Backend.Protocol.Types (EncoinsMode (..))

protocolFeeWalletMode :: Integer -> Integer
protocolFeeWalletMode v
    | v >= 0     = 0
    | otherwise = max 2 $ negate $ v `divide` 200

protocolFeeLedgerMode :: Integer -> Integer
protocolFeeLedgerMode v
    | v >= 0     = 2
    | otherwise = max 2 $ negate $ v `divide` 200

protocolFee :: EncoinsMode -> Integer -> Integer
protocolFee mode v = case mode of
    WalletMode   -> protocolFeeWalletMode v
    LedgerMode   -> protocolFeeLedgerMode v

protocolFees :: EncoinsMode -> Integer -> Integer
protocolFees mode = (*2) . protocolFee mode
