{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module I18n.Common.I18n where

-- import Common.Prelude
import Control.Applicative as A (Const (..))
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString)
import qualified Data.Text as T
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Data.Time 
import GHC.Generics (Generic)
import Data.Coerce (coerce)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Numeric.Natural (Natural)
import Text.Printf
import Common.Utility (toText)

-- | A type class capturing internationalization (i18n) for arbitrary @term@s and @locale@s.
--
-- An instance of @HasI18n locale term target@ proves that some 'term' type can be localized
-- with some 'locale' type to some given 'target' type.
--
-- For example:
--
-- > data Locale = EN | ES
-- > data Term = Hello | Goodbye
-- > instance HasI18n Locale Term Text where
-- >   localizeWith EN Hello   = "Hello"
-- >   localizeWith EN Goodbye = "Goodbye"
-- >   localizeWith ES Hello   = "Hola"
-- >   localizeWith ES Goodbye = "Adiós"
class HasI18n locale term target where
  localizeWith :: locale -> term -> target

data Locale = Locale_EN | Locale_ZH
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance HasI18n Locale Locale Text where
  localizeWith _ Locale_EN = "English"
  localizeWith _ Locale_ZH = "中文"

instance HasI18n Locale UTCTime Text where
  localizeWith locale t = case locale of
    Locale_EN -> T.pack $ formatTime defaultTimeLocale "%D" t
    Locale_ZH -> T.pack $ formatTime defaultTimeLocale "%Y年%-m月%-d日" t -- FIXME: ZH TimeLocale

-- | Handy function when we want to localize only English.
localizeENWith :: HasI18n Locale a Text => (a -> Text) -> Locale -> a -> Text
localizeENWith f = \case
  Locale_EN -> f
  Locale_ZH -> localizeWith Locale_EN -- Not translating Chinese yet

instance HasI18n Locale Day Text where
  localizeWith _locale = T.pack . show

-- | Treat 'Text' values as already being localized.
instance HasI18n Locale Text Text where
  localizeWith _ = id

instance HasI18n Locale Int Text where
  localizeWith _ = toText

instance HasI18n Locale Natural Text where
  localizeWith _ = toText

instance HasI18n Locale Double Text where
  localizeWith _ = T.pack . printf "%.4f"

instance HasI18n Locale Bool Text where
  localizeWith _ b = if b then "Yes" else "No"

instance HasI18n Locale a Text => HasI18n Locale (Maybe a) Text where
  localizeWith locale = \case
    Nothing -> T.empty
    Just v -> localizeWith locale v

instance (HasI18n Locale a Text, HasI18n Locale b Text) => HasI18n Locale (a, b) Text where
  localizeWith locale (x, y) = localizeWith locale x <> " " <> localizeWith locale y

instance HasI18n Locale a Text => HasI18n Locale [a] Text where
  localizeWith locale = T.intercalate ", " . map (localizeWith locale)

instance HasI18n Locale a Text => HasI18n Locale (Set a) Text where
  localizeWith locale = localizeWith locale . Set.toList

instance HasI18n Locale a Text => HasI18n Locale (NonEmpty a) Text where
  localizeWith locale = localizeWith locale . NE.toList

class HasI18n Locale t Text => Localized t

instance HasI18n Locale t Text => Localized t

standardLocalize :: (Locale -> f a -> Text) -> Locale -> f a -> Text
standardLocalize f locale term = case locale of
  Locale_EN -> f locale term
  _ -> f locale term

-- | How to localize the factor *values*
class LocalizeFactor f x where
  localizeFactor :: Locale -> f a -> a -> x

-- | Given some higher-kinded data, it's nice to be able to localize field names without dealing with the entire
-- datatype represetned as Text.  The following takes a locale and some `hkd` and prduces localized `term`
-- in the shape of the original data.
-- requires -XAllowAmbiguousTypes to define and -XTypeApplications to use
-- For example:
--
-- > data Locale = EN | ES
-- > data Foo f = Foo {foo :: f Double, bar :: f String}
-- > engFoo = Foo (Const "the Double foo") (Const "The string bar")
-- > instance HasI18nFields Foo f Locale Text where
-- >   localizedFields EN = engFoo
-- > getConst $ localizedFields @Foo EN ^. foo
-- > -- "the Double foo"
class HasI18nFields (hkd :: (Type -> Type) -> Type) (f :: Type -> Type) locale term where
  localizedFields :: locale -> hkd (Const term)

-- | Utilities for names of Fields that are effectively `Text`
newtype FieldName = FieldName {unFieldName :: Text}
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving newtype (IsString, Monoid, Semigroup)

constFieldName :: Text -> Const FieldName a
constFieldName = coerce

fieldNameConst :: Const FieldName a -> Text
fieldNameConst = coerce

capitalizeConstTextHead :: Const FieldName a -> Const FieldName a
capitalizeConstTextHead (Const (FieldName t)) = constFieldName $ (T.toUpper . T.singleton $ T.head t) <> T.drop 1 t

-- -- | Dumbly renders field names of `Product`s.  Foo (Mean `Product` Sum) =>  "Mean foo by Sum foo"
-- instance
--   ( HasI18nFields hkd a Locale FieldName,
--     HasI18nFields hkd b Locale FieldName,
--     ApplicativeB hkd
--   ) =>
--   HasI18nFields hkd (Product a b) Locale FieldName
--   where
--   localizedFields l = bzipWith (\a b -> a <> " by " <> b) (localizedFields @hkd @a l) (localizedFields @hkd @b l)
