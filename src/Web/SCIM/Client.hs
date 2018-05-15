module Web.SCIM.Client where

import           Web.SCIM.Server
import           Servant
import           Servant.Client
import           Web.SCIM.Schema.User (User)
import           Web.SCIM.Schema.ListResponse (ListResponse)
import           Web.SCIM.Class.Group (Group, StoredGroup)
import           Web.SCIM.Class.User (StoredUser)
import           Web.SCIM.Capabilities.MetaSchema (Configuration)
import           Data.Text (Text)
import           Data.Aeson (Value)


api :: Proxy SiteAPI
api = Proxy


resourceTypes :: ClientM Text
schema :: Text -> ClientM Value
schemas :: ClientM [Value]
-- | get server configuration
spConfig :: ClientM Configuration
-- | list all stored users
listUsers :: ClientM (ListResponse StoredUser)
-- | create user
postUser :: User -> ClientM StoredUser
-- | lookup user
lookupUser :: Text -> ClientM StoredUser
-- | update user
putUser :: Text -> User -> ClientM StoredUser
patchUser :: Text -> ClientM StoredUser
deleteUser :: Text -> ClientM NoContent
listGroups :: ClientM [StoredGroup]
getGroup :: Text -> ClientM StoredGroup
postGroup :: Group -> ClientM StoredGroup
putGroup :: Text -> Group -> ClientM StoredGroup
patchGroup :: Text -> ClientM StoredGroup
deleteGroup :: Text -> ClientM NoContent

-- TODO: is it worth using servant-generic? The structure of the
-- generated route seems terrible
(((spConfig :<|> schemas) :<|> schema :<|> resourceTypes) :<|>
 ((listUsers :<|> lookupUser :<|> postUser) :<|> putUser :<|> patchUser :<|> deleteUser)
 :<|> ((listGroups :<|> (getGroup :<|> postGroup)) :<|> putGroup :<|> patchGroup :<|> deleteGroup))
  = client api
