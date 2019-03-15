{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Scim.Class.Group
  ( GroupSite (..)
  , GroupDB (..)
  , GroupTypes (..)
  , StoredGroup
  , Group (..)
  , Member (..)
  , groupServer
  ) where

import           Control.Monad
import           Data.Text
import           Data.Aeson
import           GHC.Generics (Generic)
import           Web.Scim.Schema.Common
import           Web.Scim.Schema.Error
import           Web.Scim.Schema.Meta
import           Web.Scim.ContentType
import           Web.Scim.Handler
import           Web.Scim.Class.Auth
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

----------------------------------------------------------------------------
-- /Groups API

type Schema = Text

-- | Configurable parts of 'Group'.
class GroupTypes tag where
  -- | Group ID type.
  type GroupId tag

-- TODO
data Member = Member
  { value :: Text
  , typ :: Text
  , ref :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Member where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Member where
  toJSON = genericToJSON serializeOptions

data Group = Group
  { schemas :: [Schema]
  , displayName :: Text
  , members :: [Member]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Group where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Group where
  toJSON = genericToJSON serializeOptions

type StoredGroup tag = WithMeta (WithId (GroupId tag) Group)

data GroupSite tag route = GroupSite
  { getGroups :: route :-
      Get '[SCIM] [StoredGroup tag]
  , getGroup :: route :-
      Capture "id" (GroupId tag) :>
      Get '[SCIM] (StoredGroup tag)
  , postGroup :: route :-
      ReqBody '[SCIM] Group :>
      PostCreated '[SCIM] (StoredGroup tag)
  , putGroup :: route :-
      Capture "id" (GroupId tag) :>
      ReqBody '[SCIM] Group :>
      Put '[SCIM] (StoredGroup tag)
  , patchGroup :: route :-
      Capture "id" (GroupId tag) :>
      Patch '[SCIM] (StoredGroup tag)
  , deleteGroup :: route :-
      Capture "id" (GroupId tag) :>
      DeleteNoContent '[SCIM] NoContent
  } deriving (Generic)

----------------------------------------------------------------------------
-- Methods used by the API

class (Monad m, GroupTypes tag, AuthDB tag m) => GroupDB tag m where
  list :: AuthInfo tag -> ScimHandler m [StoredGroup tag]
  get :: AuthInfo tag -> GroupId tag -> ScimHandler m (Maybe (StoredGroup tag))
  create :: AuthInfo tag -> Group -> ScimHandler m (StoredGroup tag)
  update :: AuthInfo tag -> GroupId tag -> Group -> ScimHandler m (StoredGroup tag)
  delete :: AuthInfo tag -> GroupId tag -> ScimHandler m Bool  -- ^ Return 'False' if the group didn't exist

----------------------------------------------------------------------------
-- API handlers

groupServer
    :: forall tag m. (Show (GroupId tag), GroupDB tag m)
    => Maybe (AuthData tag) -> GroupSite tag (AsServerT (ScimHandler m))
groupServer authData = GroupSite
  { getGroups = do
      auth <- authCheck @tag authData
      getGroups' @tag auth
  , getGroup = \gid -> do
      auth <- authCheck @tag authData
      getGroup' @tag auth gid
  , postGroup = \gr -> do
      auth <- authCheck @tag authData
      postGroup' @tag auth gr
  , putGroup = \gid gr -> do
      auth <- authCheck @tag authData
      putGroup' @tag auth gid gr
  , patchGroup = error "PATCH /Groups: not implemented"
  , deleteGroup = \gid -> do
      auth <- authCheck @tag authData
      deleteGroup' @tag auth gid
  }

getGroups'
    :: forall tag m. GroupDB tag m
    => AuthInfo tag -> ScimHandler m [StoredGroup tag]
getGroups' auth = do
  list @tag auth

getGroup'
    :: forall tag m. (Show (GroupId tag), GroupDB tag m)
    => AuthInfo tag -> GroupId tag -> ScimHandler m (StoredGroup tag)
getGroup' auth gid = do
  maybeGroup <- get @tag auth gid
  maybe (throwScim (notFound "Group" (pack (show gid)))) pure maybeGroup

postGroup'
    :: forall tag m. GroupDB tag m
    => AuthInfo tag -> Group -> ScimHandler m (StoredGroup tag)
postGroup' auth gr = do
  create @tag auth gr

putGroup'
    :: forall tag m. GroupDB tag m
    => AuthInfo tag -> GroupId tag -> Group -> ScimHandler m (StoredGroup tag)
putGroup' auth gid gr = do
  update @tag auth gid gr

deleteGroup'
    :: forall tag m. (Show (GroupId tag), GroupDB tag m)
    => AuthInfo tag -> GroupId tag -> ScimHandler m NoContent
deleteGroup' auth gid = do
  deleted <- delete @tag auth gid
  unless deleted $ throwScim (notFound "Group" (pack (show gid)))
  pure NoContent
