{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings, RecordWildCards #-}

import Data.Monoid ((<>))
import Control.Monad (join, when)
import Data.List (lookup)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Yesod
import System.Environment (getEnv)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (decode)
import Text.InterpolatedString.Perl6
import Data.Aeson.TH
import Network.Wreq
import Control.Lens
import Misc

data Project = Project { clientId :: Text
                       , clientSecret :: Text
                       , url :: Text
                       }

instance Yesod Project where
  -- Get app URL from environment.
  approot = ApprootMaster url

data GithubResponse = GithubResponse { accessToken :: Text
                                     , scope :: [Text]
                                     , tokenType :: Text
                                     }
                      deriving (Show)

$(deriveJSON defaultOptions { constructorTagModifier = underscore } ''GithubResponse)

mkYesod "Project" [parseRoutes|
                   / HomeR GET
                   /private PrivateR GET
                  |]

getHomeR :: Handler Html
getHomeR = do
  -- Get app environment into scope.
  Project { .. } <- getYesod
  -- Get request info.
  req <- getRequest
  -- Send our user to GitHub.
  redirect ([qc|https://github.com/login/oauth/authorize?client_id={clientId}&redirect_uri={url}/private&state={fromJust $ reqToken req}|] :: Text)

getPrivateR :: Handler Html
getPrivateR = do
  Project { .. } <- getYesod
  req <- getRequest
  -- Lookup code and state from request.
  (code, state) <- maybe (fail "GitHub should have redirected you here") return $ do
    let look n = lookup n $ reqGetParams req
    c <- look "code"
    s <- look "state"
    return (c, s)
  -- Check state
  when (state /= fromJust (reqToken req)) $ fail "Invalid state; please don't try to hack us!"
  -- Call Github back
  r <- liftIO $ postWith (defaults & header "Accept" .~ ["application/json"])
       "https://github.com/login/oauth/access_token"
       [ "client_id" := clientId
       , "client_secret" := clientSecret
       , "code" := code
       , "redirect_uri" := url <> "/private"
       ]
  a@GithubResponse { .. } <- maybe (fail "Invalid response from GitHub") return $ decode $ r ^. responseBody
  defaultLayout [whamlet| Response: #{show a}
                        |]

main = do
  clientId <- T.pack <$> getEnv "CLIENT_ID"
  clientSecret <- T.pack <$> getEnv "CLIENT_SECRET"
  url <- T.pack <$> getEnv "APP_URL"
  -- Start server with the environment.
  warp 3000 Project { .. }
