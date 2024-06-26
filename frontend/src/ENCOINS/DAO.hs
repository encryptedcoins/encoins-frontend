module ENCOINS.DAO
    ( dao
    ) where

import Reflex.Dom

import ENCOINS.DAO.Body (bodyWidget)
import ENCOINS.Common.Head (headWidget)

dao :: IO ()
dao = mainWidgetWithHead headWidget bodyWidget
