
module Web.Scim.Schema.User.Name where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import Web.Scim.Class.Patch

import Web.Scim.Schema.Common

data Name = Name
  { formatted :: Maybe Text
  , familyName :: Maybe Text
  , givenName :: Maybe Text
  , middleName :: Maybe Text
  , honorificPrefix :: Maybe Text
  , honorificSuffix :: Maybe Text
  } deriving (Show, Eq, Generic)

empty :: Name
empty = Name Nothing Nothing Nothing Nothing Nothing Nothing

instance FromJSON Name where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Name where
  toJSON = genericToJSON serializeOptions

instance SubAttrLens (Maybe Name) where
  subAttrLens field = 
    case field of
      "formatted" ->
        pure $ GetSet (formatted =<<) $ \mName mText -> do
          -- if the name is unset, and the text is null, keep name null
          -- TODO(arianvp): Check if this is according to spec
          case (mName, mText) of
            (Nothing, Nothing) -> pure $ Nothing
            (Nothing, Just x) -> pure $ Just $ empty { formatted = Just x}
            (Just m, Just x) -> pure $ Just $ m { formatted = Just x }
      "familyname" ->
        pure $ GetSet (familyName =<<) $ \mName mText -> do
          -- if the name is unset, and the text is null, keep name null
          -- TODO(arianvp): Check if this is according to spec
          case (mName, mText) of
            (Nothing, Nothing) -> pure $ Nothing
            (Nothing, Just x) -> pure $ Just $ empty { familyName = Just x}
            (Just m, Just x) -> pure $ Just $ m { familyName = Just x }
      "givenname" ->
        pure $ GetSet (givenName =<<) $ \mName mText -> do
          -- if the name is unset, and the text is null, keep name null
          -- TODO(arianvp): Check if this is according to spec
          case (mName, mText) of
            (Nothing, Nothing) -> pure $ Nothing
            (Nothing, Just x) -> pure $ Just $ empty { givenName = Just x}
            (Just m, Just x) -> pure $ Just $ m { givenName = Just x }
      "middlename" ->
        pure $ GetSet (middleName =<<) $ \mName mText -> do
          -- if the name is unset, and the text is null, keep name null
          -- TODO(arianvp): Check if this is according to spec
          case (mName, mText) of
            (Nothing, Nothing) -> pure $ Nothing
            (Nothing, Just x) -> pure $ Just $ empty { middleName = Just x}
            (Just m, Just x) -> pure $ Just $ m { middleName = Just x }
      "honorificprefix" ->
        pure $ GetSet (honorificPrefix =<<) $ \mName mText -> do
          -- if the name is unset, and the text is null, keep name null
          -- TODO(arianvp): Check if this is according to spec
          case (mName, mText) of
            (Nothing, Nothing) -> pure $ Nothing
            (Nothing, Just x) -> pure $ Just $ empty { honorificPrefix = Just x}
            (Just m, Just x) -> pure $ Just $ m { honorificPrefix = Just x }
      "honorificsuffix" ->
        pure $ GetSet (honorificSuffix =<<) $ \mName mText -> do
          -- if the name is unset, and the text is null, keep name null
          -- TODO(arianvp): Check if this is according to spec
          case (mName, mText) of
            (Nothing, Nothing) -> pure $ Nothing
            (Nothing, Just x) -> pure $ Just $ empty { honorificSuffix = Just x}
            (Just m, Just x) -> pure $ Just $ m { honorificSuffix = Just x }
