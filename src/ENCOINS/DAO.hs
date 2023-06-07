module ENCOINS.DAO (dao) where

import           Reflex.Dom

import           ENCOINS.Website.Head        (headWidget)
import           ENCOINS.DAO.Body            (bodyWidget)

dao :: IO ()
dao = mainWidgetWithHead headWidget bodyWidget