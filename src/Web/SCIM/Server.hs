{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Web.SCIM.Server
  ( app
  , SiteAPI
  , mkapp, App

  -- * API subtrees, useful for tests
  , ConfigAPI, configServer
  , UserAPI, userServer
  , GroupAPI, groupServer
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Web.SCIM.Class.User (UserSite (..), UserDB, userServer)
import           Web.SCIM.Class.Group (GroupSite (..), GroupDB, groupServer)
import           Web.SCIM.Class.Auth (Admin, AuthDB (..))
import           Web.SCIM.Schema.Error
import           Web.SCIM.Capabilities.MetaSchema (ConfigSite, Configuration, configServer)
import           Control.Monad.Catch hiding (Handler)
import           Control.Monad.Except
import           GHC.Generics (Generic)
import           Network.Wai
import           Servant hiding (BasicAuth, NoSuchUser)
import           Servant.Generic
import           Servant.Auth
import           Servant.Auth.Server
#if !MIN_VERSION_servant_server(0,12,0)
import           Servant.Utils.Enter
#endif

----------------------------------------------------------------------------
-- API specification

type SCIMHandler m = (MonadThrow m, UserDB m, GroupDB m, AuthDB m)

type ConfigAPI = ToServant (ConfigSite AsApi)
type UserAPI   = ToServant (UserSite AsApi)
type GroupAPI  = ToServant (GroupSite AsApi)
type SiteAPI   = ToServant (Site AsApi)

data Site route = Site
  { config :: route :-
      ConfigAPI
  , users :: route :-
      Auth '[BasicAuth] Admin :>
      "Users" :> UserAPI
  , groups :: route :-
      Auth '[BasicAuth] Admin :>
      "Groups" :> GroupAPI
  } deriving (Generic)

----------------------------------------------------------------------------
-- API implementation

-- TODO: this is horrible, let's switch to servant-server-0.12 as soon as possible
#if MIN_VERSION_servant_server(0,12,0)
type EnterBoilerplate m = ( Functor m )  -- `()` is parsed as a type
#else
type EnterBoilerplate m =
  ( Enter (ServerT UserAPI (Either ServantErr)) (Either ServantErr) m (ServerT UserAPI m)
  , Enter (ServerT GroupAPI (Either ServantErr)) (Either ServantErr) m (ServerT GroupAPI m)
  )
#endif

siteServer ::
  forall m. (SCIMHandler m, EnterBoilerplate m) =>
  Configuration -> Site (AsServerT m)
siteServer conf = Site
  { config = toServant $ configServer conf
  , users = \authResult -> case authResult of
      Authenticated _ -> toServant userServer
#if MIN_VERSION_servant_server(0,12,0)
      _ -> hoistServer (Proxy @UserAPI) nt (noAuth authResult)
#else
      _ -> enter (NT nt) (noAuth authResult)
#endif
  , groups = \authResult -> case authResult of
      Authenticated _ -> toServant groupServer
#if MIN_VERSION_servant_server(0,12,0)
      _ -> hoistServer (Proxy @GroupAPI) nt (noAuth authResult)
#else
      _ -> enter (NT nt) (noAuth authResult)
#endif
  }
  where
    noAuth res = throwAll $ err401 { errBody = BL.pack (show res) }
    -- Due to overlapping instances, we can't just use 'throwAll' to get a
    -- @ServerT ... m@. Instead we first generate a very simple @ServerT api
    -- (Either ServantErr)@ and hoist it to become @ServerT api m@.
    --
    -- @Either ServantErr@ is the simplest type that is supported by 'throwAll'
    -- and that can be converted into a @MonadThrow m@.
    nt :: Either ServantErr a -> m a
    nt = either throwM pure

----------------------------------------------------------------------------
-- Server-starting utilities

type App m api = ( SCIMHandler m
                 , HasServer api '[JWTSettings, CookieSettings, BasicAuthCfg]
                 , EnterBoilerplate m
#if !MIN_VERSION_servant_server(0,12,0)
                 , Enter (ServerT api m) m Handler (Server api)
#endif
                 )

mkapp :: forall m api. (App m api)
      => Proxy api
      -> ServerT api m
      -> (forall a. m a -> IO a)    -- Usually the transformation is "m a -> Handler a",
                                    -- but in this library we give up on Servant's
                                    -- 'MonadError' and just throw things via IO
      -> IO Application
mkapp proxy api toIO = do
  jwtKey <- generateKey
  authCfg <- either throwM pure =<< runHandler (nt mkAuthChecker)
  let jwtCfg = defaultJWTSettings jwtKey
      cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
  pure $ serveWithContext proxy cfg $
#if MIN_VERSION_servant_server(0,12,0)
    hoistServerWithContext proxy (Proxy @[JWTSettings, CookieSettings, BasicAuthCfg]) nt api
#else
    enter (NT nt) api
#endif
  where
    -- Our handlers can throw two kinds of exceptions that we care about:
    -- ServantErr and SCIMError. We catch both and rethrow them via ExceptT
    -- (which is how Servant expects to get them).
    nt :: forall a. m a -> Handler a
    nt act = Handler $ ExceptT $
      fmap Right (toIO act)
        `catch` (\(e :: SCIMError) -> pure (Left (scimToServantErr e)))
        `catch` (\(e :: ServantErr) -> pure (Left e))

app :: forall m. App m SiteAPI => Configuration -> (forall a. m a -> IO a) -> IO Application
app c = mkapp (Proxy @SiteAPI) (toServant $ siteServer c)
