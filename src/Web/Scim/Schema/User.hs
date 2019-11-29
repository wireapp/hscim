{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SCIM user representation.
--
-- = Our interpretation of the spec
--
-- The spec can be read at <https://tools.ietf.org/html/rfc7643#section-4.1>.
-- While implementing the spec we had to resolve some ambiguities and place some
-- additional constraints on the possible SCIM server behavior we can support.
--
-- == Resource ID / user ID
--
-- The 'User' object doesn't contain a user ID (as in "opaque server-assigned
-- immutable ID") by design. IDs and metadata are added to types in a uniform
-- fashion by using @WithId@ and @WithMeta@.
--
-- == Optional fields
--
-- The spec only mandates the @userName@ and @id@ attribute. All other
-- attributes seem optional.
--
-- == Multi-valued fields
--
-- When a multi-valued field (e.g. @emails@) doesn't contain any values, it's
-- unclear whether we should serialize it as @[]@ or omit it entirely. We have
-- opted for the latter to conform to an example in the spec:
-- <https://tools.ietf.org/html/rfc7644#section-3.5.1>.
--
-- TODO(arianvp):
--  Multi-valued attributes actually have some more quirky semantics that we
--  currently' don't support yet. E.g. if the multi-values have a
--  'primary' field then only one of the entires must have 'primary: true'
--  and all the others are either implied 'primary: false' or must be checked
--  that they're false
--
--
-- == Attribute names
--
-- When parsing JSON objects, we ignore capitalization differences in field
-- names -- e.g. both @USERNAME@ and @userName@ are accepted.
--  This is described by the spec  https://tools.ietf.org/html/rfc7643#section-2.1
--
module Web.Scim.Schema.User where

import Data.Proxy
import Data.Dynamic
import Control.Monad (foldM)
import Data.Text (Text, toLower, toCaseFold)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Lens.Micro 

import Web.Scim.Filter (Filter(..), AttrPath(..), rAttrName, compareStr, AttrName, CompValue(..))
import Web.Scim.Schema.Common
import Web.Scim.Schema.Schema (Schema(..))
import Web.Scim.Schema.PatchOp (Path(..), PatchOp, Op(..), Operation(Operation), getOperations)
import Web.Scim.Schema.User.Address (Address)
import Web.Scim.Schema.User.Certificate (Certificate)
import Web.Scim.Schema.User.Email (Email)
import Web.Scim.Schema.User.IM (IM)
import Web.Scim.Schema.User.Name (Name)
import Web.Scim.Schema.User.Phone (Phone)
import Web.Scim.Schema.User.Photo (Photo)
import Web.Scim.Schema.Error
import Web.Scim.Class.Patch

import GHC.Generics (Generic)

-- | Configurable parts of 'User'.
class UserTypes tag where
  -- | User ID type.
  type UserId tag
  -- | Extra data carried with each 'User'.
  type UserExtra tag

-- | SCIM user record, parametrized with type-level tag @t@ (see 'UserTypes').
data User tag = User
  {
    schemas :: [Schema]

  -- Mandatory fields
  , userName :: Text

  -- Optional fields
  , externalId :: Maybe Text
  , name :: Maybe Name
  , displayName :: Maybe Text
  , nickName :: Maybe Text
  , profileUrl :: Maybe URI
  , title :: Maybe Text
  , userType :: Maybe Text
  , preferredLanguage :: Maybe Text
  , locale :: Maybe Text
  , active :: Maybe Bool
  , password :: Maybe Text

  -- Multi-valued fields
  , emails :: [Email]
  , phoneNumbers :: [Phone]
  , ims :: [IM]
  , photos :: [Photo]
  , addresses :: [Address]
  , entitlements :: [Text]
  , roles :: [Text]
  , x509Certificates :: [Certificate]

  -- Extra data.
  --
  -- During rendering, we'll convert it to JSON; if it's an object we'll merge it with the
  -- main user object, if it's @null@ we'll do nothing, otherwise we'll add it under the
  -- @"extra"@ field (though you should definitely not rely on this).
  --
  -- During parsing, we'll attempt to parse the /whole/ user object as @extra@, so your
  -- 'FromJSON' instance should be prepared to ignore unrelated fields. Also keep in mind that
  -- the SCIM spec requires field names to be case-insensitive, i.e. if you're looking for a
  -- field "foo" you should also handle a field called "FOO". Look at the @FromJSON User@
  -- instance to see how it can be done.
  --
  -- FUTUREWORK: make it easy for hscim users to implement a proper parser (with correct
  -- rendering of optional and multivalued fields, lowercase objects, etc).
  , extra :: UserExtra tag
  } deriving (Generic, Typeable)

deriving instance Show (UserExtra tag) => Show (User tag)
deriving instance Eq (UserExtra tag) => Eq (User tag)

instance PathLens (User tag) where
  pathLens (AttrPath schema attrName subAttr) =
    -- TODO(arianvp): Support all fields
    -- FUTUREWORK(arianvp): Generate this with Generics. This is purely mechanical
    case attrName of
      "username" -> pure $ GetSet userName (\x value -> pure $ x { userName = value})
      "displayname" -> pure $ GetSet displayName (\x value -> pure $ x { displayName = value})
      "externalid" -> pure $ GetSet externalId (\x value -> pure $ x { externalId = value})
      -- Example of how to set a nested field
      "name" ->
        case subAttr of
          Nothing -> pure $ GetSet name (\x value -> pure $ x { name = value })
          Just x -> do
            GetSet getX setX <- subAttrLens x
            pure $ GetSet (getX . name) $ \y value -> do
              value' <- setX (name y) value
              pure $ y { name = value' }
      -- unsupported fields do not modify the user for now.  TODO(arianvp): there shouldn't be unsupported fields
      -- TODO(arianvp) implement
      _ -> undefined



empty
  :: [Schema]               -- ^ Schemas
  -> UserExtra tag          -- ^ Extra data
  -> User tag
empty schemas extra = User
  { schemas = schemas
  , userName = ""
  , externalId = Nothing
  , name = Nothing
  , displayName = Nothing
  , nickName = Nothing
  , profileUrl = Nothing
  , title = Nothing
  , userType = Nothing
  , preferredLanguage = Nothing
  , locale = Nothing
  , active = Nothing
  , password = Nothing
  , emails = []
  , phoneNumbers = []
  , ims = []
  , photos = []
  , addresses = []
  , entitlements = []
  , roles = []
  , x509Certificates = []
  , extra = extra
  }


instance FromJSON (UserExtra tag) => FromJSON (User tag) where
  parseJSON = withObject "User" $ \obj -> do
    -- Lowercase all fields
    let o = HM.fromList . map (over _1 toLower) . HM.toList $ obj
    schemas <- o .:? "schemas" <&> \case
        Nothing -> [User20]
        Just xs -> if User20 `elem` xs then xs else User20 : xs
    userName <- o .: "username"
    externalId <- o .:? "externalid"
    name <- o .:? "name"
    displayName <- o .:? "displayname"
    nickName <- o .:? "nickname"
    profileUrl <- o .:? "profileurl"
    title <- o .:? "title"
    userType <- o .:? "usertype"
    preferredLanguage <- o .:? "preferredlanguage"
    locale <- o .:? "locale"
    active <- o .:? "active"
    password <- o .:? "password"
    emails <- o .:? "emails" .!= []
    phoneNumbers <- o .:? "phonenumbers" .!= []
    ims <- o .:? "ims" .!= []
    photos <- o .:? "photos" .!= []
    addresses <- o .:? "addresses" .!= []
    entitlements <- o .:? "entitlements" .!= []
    roles <- o .:? "roles" .!= []
    x509Certificates <- o .:? "x509certificates" .!= []

    -- NOTE: We pass in the _original_ object. This is because
    -- our schema URN parser is currently case-sensitive (Eventhough SCIM
    -- mandates we should be case-insensitive).
    -- This means we're not fully compliant here
    -- TODO(arianvp): Think of making URN parsing case-insensitive
    extra <- parseJSON (Object obj)
    pure User{..}

instance ToJSON (UserExtra tag) => ToJSON (User tag) where
  toJSON User{..} =
    let mainObject = HM.fromList $ concat
          [ [ "schemas" .= schemas ]
          , [ "userName" .= userName ]
          , optionalField "externalId" externalId
          , optionalField "name" name
          , optionalField "displayName" displayName
          , optionalField "nickName" nickName
          , optionalField "profileUrl" profileUrl
          , optionalField "title" title
          , optionalField "userType" userType
          , optionalField "preferredLanguage" preferredLanguage
          , optionalField "locale" locale
          , optionalField "active" active
          , optionalField "password" password
          , multiValuedField "emails" emails
          , multiValuedField "phoneNumbers" phoneNumbers
          , multiValuedField "ims" ims
          , multiValuedField "photos" photos
          , multiValuedField "addresses" addresses
          , multiValuedField "entitlements" entitlements
          , multiValuedField "roles" roles
          , multiValuedField "x509Certificates" x509Certificates
          ]
        extraObject = case toJSON extra of
          Null -> mempty
          Object x -> x
          other -> HM.fromList ["extra" .= other]
    in Object (HM.union mainObject extraObject)

    where
      -- Omit a field if it's Nothing
      optionalField fname = \case
        Nothing -> []
        Just x  -> [fname .= x]
      -- Omit a field if it's []
      multiValuedField fname = \case
        [] -> []
        xs -> [fname .= xs]

-- | A type used to indicate that the SCIM record doesn't have any extra data. Encoded as an
-- empty map.
data NoUserExtra = NoUserExtra
  deriving (Eq, Show)

instance FromJSON NoUserExtra where
  parseJSON = withObject "NoUserExtra" $ \_ -> pure NoUserExtra

instance ToJSON NoUserExtra where
  toJSON _ = object []

----------------------------------------------------------------------------
-- Applying

-- | Applies a JSON Patch to a SCIM Core User
-- Only supports the core attributes. 
-- Evenmore, only some hand-picked ones currently.
-- We'll have to think how patch is going to work in the presence of extensions.
-- Also, we can probably make  PatchOp type-safe to some extent (Read arianvp's thesis :))
--
--

applyPatch :: User tag  -> PatchOp -> Either ScimError (User tag)
applyPatch = (. getOperations) . foldM applyOperation


-- TODO(arianvp): support multi-valuued and complex attributes.
-- TODO(arianvp): Actually do this in some kind of type-safe way. e.g.
-- have a UserPatch type
--
-- What I understand from the spec:  The difference between add an replace is only
-- in the fact that replace will not concat multi-values, and behaves differently for complex values too.
-- For simple attributes, add and replace are identical
applyOperation :: forall tag. User tag  -> Operation -> Either ScimError (User tag)
applyOperation user (Operation op path value) = 
  case op of
    Add ->
      let
        kvToPath :: (Text, Value) -> Path
        kvToPath = undefined
        applyAdd :: User tag -> Path -> Either ScimError (User tag)
        applyAdd u (NormalPath attrPath) = undefined
        applyAdd u (IntoValuePath _ _)  = Left undefined -- no value paths supported yet

      in case path of
         -- o If omitted, the target location is assumed to be the resource
         --   itself.  The "value" parameter contains a set of attributes to be
         --   added to the resource.
         --
        Nothing ->
          case value of
            Object o -> undefined
              --foldM _ user (HM.toList o)
            _ -> Left undefined -- probably throw some error
            
        Just path -> applyAdd user path
          -- o  If the target location does not exist, the attribute and value are
          --      added.
          --
          --   NOTE(arianvp): Add a test for this. This sounds important.
          --   Currently in Spar, this constraint is fulfilled by the fact
          --   that PUT does this already and our PATCH is implemented in terms of PUT.
          --   TODO(arianvp): It would be nicer if this updating of the modified metadata
          --    is pushed to the library level instead though!
          --   o  If the target location already contains the value specified, no
          --      changes SHOULD be made to the resource, and a success response
          --      SHOULD be returned.  Unless other operations change the resource,
          --      this operation SHALL NOT change the modify timestamp of the
          --      resource.
    Replace ->
      case path of
       Nothing -> undefined
       Just path -> undefined
    Remove -> undefined


-- | Check whether a user satisfies the filter.
--
-- Returns 'Left' if the filter is constructed incorrectly (e.g. tries to
-- compare a username with a boolean).
--
-- TODO(arianvp): We need to generalise filtering at some point probably.
filterUser :: Filter -> User extra -> Either Text Bool
filterUser (FilterAttrCompare (AttrPath schema attrib subAttr) op val) user
  | isUserSchema schema =
      case (subAttr, val) of
        (Nothing, (ValString str)) | rAttrName attrib == "username" ->
          Right (compareStr op (toCaseFold (userName user)) (toCaseFold str))
        (Nothing, _) | rAttrName attrib == "username" ->
          Left "usernames can only be compared with strings"
        (_, _) ->
          Left "Only search on usernames is currently supported"
  | otherwise = Left "Invalid schema. Only user schema is supported"

-- Omission of a schema for users is implicitly the core schema
-- TODO(arianvp): Link to part of the spec that claims this.
isUserSchema Nothing = True
isUserSchema (Just User20) = True
isUserSchema _ = False
