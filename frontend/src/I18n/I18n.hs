module I18n.I18n where

import I18n.App (AppMessage)
import I18n.Common (CommonMessage, WelcomeMessage)
import I18n.Dao (DaoMessage)
import I18n.Landing (LandMessage)
import I18n.Reflex.I18n (HasI18n (localizeWith), HasLocale, Locale)

import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom

type App t m = (MonadWidget t m, HasLocale t Locale m)

data LocalizedMessage
    = EmptyTerm
    | AppTerm AppMessage
    | DaoTerm DaoMessage
    | LandTerm LandMessage
    | CommonTerm CommonMessage
    | WelcomeTerm WelcomeMessage
    deriving stock (Eq, Show)

instance HasI18n Locale LocalizedMessage Text where
    localizeWith locale = \case
        EmptyTerm -> T.empty
        AppTerm t -> localizeWith locale t
        DaoTerm t -> localizeWith locale t
        LandTerm t -> localizeWith locale t
        CommonTerm t -> localizeWith locale t
        WelcomeTerm t -> localizeWith locale t
