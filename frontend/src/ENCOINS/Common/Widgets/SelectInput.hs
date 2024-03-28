module ENCOINS.Common.Widgets.SelectInput where

import           Data.List            (elemIndex)
import           Data.Text            (Text, unpack)
import           Reflex.Dom
import           Text.Read            (readMaybe)
import           Witherable           (catMaybes)

import           Backend.Utility      (toText)
import           ENCOINS.Common.Utils (safeIndex)

-- TODO: complete and move this to ENCOINS.App.Widgets
-- Title of the input element along with a hint about the expected input
inputTitle :: MonadWidget t m => Text -> Text -> m ()
inputTitle title hint = divClass "" $ do
  divClass colCls1 . elAttr "label" ("class" =: elAttrCls1) $ text title
  divClass colCls2 . elAttr "a" ("class" =: elAttrCls2 <>
    "title" =: hint <> "style" =: "cursor:pointer;") $ blank
  where
    colCls1 = ""
    colCls2 = ""
    elAttrCls1 = ""
    elAttrCls2 = ""

-- TODO: complete and move this to ENCOINS.App.Widgets
-- Dropdown list element
selectInput :: (MonadWidget t m, Eq a) => Text -> Text -> (a -> Text) -> a
  -> [a] -> m (Event t a)
selectInput title hint showFunc initVal valsRange = do
  inputTitle title hint
  input <- fmap (_selectElement_change . fst) $ selectElement (def
    & initialAttributes .~ ("class" =: inputCls)
    & selectElementConfig_initialValue .~ initValIdx)
    $ do
      mapM_ mkOption . zip [0..] $ valsRange
  return $ catMaybes $ parseVal <$> input
  where
    mkOption (idx::Int, val) = elAttr "option" ("value" =: toText idx) . text .
      showFunc $ val
    parseVal txt = readMaybe @Int (unpack txt) >>= safeIndex valsRange
    initValIdx = maybe "-1" toText $ elemIndex initVal valsRange
    inputCls = ""
