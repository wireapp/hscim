module Web.Scim.Class.Group (
  GroupSite (..)
  , GroupDB (..)
  , StoredGroup
  , Group (..)
  , GroupId
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

type GroupId = Text

type Schema = Text

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

type StoredGroup = WithMeta (WithId Group)

data GroupSite route = GroupSite
  { getGroups :: route :-
      Get '[SCIM] [StoredGroup]
  , getGroup :: route :-
      Capture "id" Text :> Get '[SCIM] StoredGroup
  , postGroup :: route :-
      ReqBody '[SCIM] Group :> PostCreated '[SCIM] StoredGroup
  , putGroup :: route :-
      Capture "id" Text :> ReqBody '[SCIM] Group :> Put '[SCIM] StoredGroup
  , patchGroup :: route :-
      Capture "id" Text :> Patch '[SCIM] StoredGroup
  , deleteGroup :: route :-
      Capture "id" Text :> DeleteNoContent '[SCIM] NoContent
  } deriving (Generic)

----------------------------------------------------------------------------
-- Methods used by the API

class (Monad m, AuthDB m) => GroupDB m where
  list :: AuthInfo m -> ScimHandler m [StoredGroup]
  get :: AuthInfo m -> GroupId -> ScimHandler m (Maybe StoredGroup)
  create :: AuthInfo m -> Group -> ScimHandler m StoredGroup
  update :: AuthInfo m -> GroupId -> Group -> ScimHandler m StoredGroup
  delete :: AuthInfo m -> GroupId -> ScimHandler m Bool  -- ^ Return 'False' if the group didn't exist
  getGroupMeta :: AuthInfo m -> ScimHandler m Meta

----------------------------------------------------------------------------
-- API handlers

groupServer
    :: GroupDB m
    => Maybe (AuthData m) -> GroupSite (AsServerT (ScimHandler m))
groupServer authData = GroupSite
  { getGroups = do
      auth <- authCheck authData
      getGroups' auth
  , getGroup = \gid -> do
      auth <- authCheck authData
      getGroup' auth gid
  , postGroup = \gr -> do
      auth <- authCheck authData
      postGroup' auth gr
  , putGroup = \gid gr -> do
      auth <- authCheck authData
      putGroup' auth gid gr
  , patchGroup = error "PATCH /Groups: not implemented"
  , deleteGroup = \gid -> do
      auth <- authCheck authData
      deleteGroup' auth gid
  }

getGroups'
    :: GroupDB m
    => AuthInfo m -> ScimHandler m [StoredGroup]
getGroups' auth = do
  list auth

getGroup'
    :: GroupDB m
    => AuthInfo m -> GroupId -> ScimHandler m StoredGroup
getGroup' auth gid = do
  maybeGroup <- get auth gid
  maybe (throwScim (notFound "Group" gid)) pure maybeGroup

postGroup'
    :: GroupDB m
    => AuthInfo m -> Group -> ScimHandler m StoredGroup
postGroup' auth gr = do
  create auth gr

putGroup'
    :: GroupDB m
    => AuthInfo m -> GroupId -> Group -> ScimHandler m StoredGroup
putGroup' auth gid gr = do
  update auth gid gr

deleteGroup'
    :: GroupDB m
    => AuthInfo m -> GroupId -> ScimHandler m NoContent
deleteGroup' auth gid = do
  deleted <- delete auth gid
  unless deleted $ throwScim (notFound "Group" gid)
  pure NoContent