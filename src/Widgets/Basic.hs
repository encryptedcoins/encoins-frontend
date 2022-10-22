module Widgets.Basic where

import           Data.Bool              (bool)
import           Data.List              (elemIndex)
import           Data.Map               (Map)
import           Data.Text              (Text, unpack)
import           Reflex.Dom
import           Text.Read              (readMaybe)
import           Witherable             (catMaybes)

import           Widgets.Utils          (toText, safeIndex)

-- TODO: finish implementations in this module

-- Element containing the result of a JavaScript computation
elementResultJS :: MonadWidget t m => Text -> (Text -> a) -> m (Dynamic t a)
elementResultJS resId f = fmap (fmap f . value) $ inputElement $ def & initialAttributes .~ "style" =: "display:none;" <> "id" =: resId

-- Title of the input element along with a hint about the expected input
-- TODO: add help popup
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

appButton :: MonadWidget t m => Text -> Dynamic t Bool -> (a -> Map Text Text)-> Dynamic t a -> m (Event t ())
appButton title dVisible mkLinkAttrs linkState = do
  let mkVisible = bool ("style" =: "display: none;") ("class" =: mainButtonWrapperCls)
  (e, _) <- elDynAttr' "div" (mkVisible <$> dVisible) . elDynAttr' "a"
    (mkLinkAttrs <$> linkState) $ text title
  return (domEvent Click e)
  where
    mainButtonWrapperCls = ""

appButtonAttrs :: Map Text Text
appButtonAttrs = "class" =: appButtonCls <> "style" =: "cursor:pointer;"
  where
    appButtonCls = ""

appButtonAttrsDisabled :: Map Text Text
appButtonAttrsDisabled = "class" =: appButtonDisabledCls <> "disabled" =: ""
  where
    appButtonDisabledCls = ""