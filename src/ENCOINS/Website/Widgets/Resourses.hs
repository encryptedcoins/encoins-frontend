module ENCOINS.Website.Widgets.Resourses (ourResourses) where

import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Reflex.Dom

import           ENCOINS.Website.Widgets.Basic (image)

resourseButton :: MonadWidget t m => Text -> Text -> Text -> Text -> m ()
resourseButton cls lnk file w = divClass "div-image-large" $
    elAttr "a" ("href" =: lnk <> "target" =: "_blank" <> "class" =: "link w-inline-block") $
    image file ("image " `Text.append` cls) w

ourResourses :: MonadWidget t m => Text -> m ()
ourResourses w = do
    resourseButton "" "https://twitter.com/ENCOINS1" "Twitter.svg" w
    resourseButton "" "https://discord.gg/Q3gPP87Tcw" "Discord.svg" w
    resourseButton "" "https://encoins-crypto.medium.com/" "Medium.svg" w
    resourseButton "" "mailto:team@encoins.io" "Email.svg" w
    resourseButton "" "https://t.me/encoins_io" "Telegram.svg" w
    resourseButton "image-disabled" "#" "GitBook.svg" w
    resourseButton "image-disabled" "#" "GitHub.svg" w
    resourseButton "" "https://www.youtube.com/channel/UCk4QtReP4kQKfIIoWw7Q1wg" "YouTube.svg" w