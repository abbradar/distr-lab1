{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, NamedFieldPuns,
    TemplateHaskell, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

import Data.Monoid ((<>))
import Control.Monad (when, unless, liftM)
import Data.Maybe (fromJust)
import Yesod hiding (get)
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

data AuthResponse = AuthResponse { accessToken :: Text
                                 , scope :: Text
                                 , tokenType :: Text
                                 }
                  deriving (Show)

data EmailResponse = EmailResponse { email :: Text
                                   , verified :: Bool
                                   , primary :: Bool
                                   }
                   deriving (Show)

-- Derive JSON encoding-decoding for data structures
$(deriveJSON defaultOptions { fieldLabelModifier = underscore } ''AuthResponse)
$(deriveJSON defaultOptions { fieldLabelModifier = underscore } ''EmailResponse)

mkYesod "Project" [parseRoutes|
                   / HomeR GET
                   /private PrivateR GET
                  |]

getHomeR :: Handler Html
getHomeR = do
  let scopes = T.intercalate "," [ "user" ]
  -- Get app environment into scope.
  Project { .. } <- getYesod
  -- Get request info.
  req <- getRequest
  -- Send our user to GitHub.
  redirect
    ([qc|https://github.com/login/oauth/authorize?client_id={clientId}&redirect_uri={url}/private&scope={scopes}&state={fromJust $ reqToken req}|] :: Text)

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
  -- Call Github back and decode response
  let dec err p = maybe (fail err) return $ decode $ p ^. responseBody
  auth@AuthResponse { accessToken } <-
    liftIO (postWith (defaults & header "Accept" .~ ["application/json"])
            "https://github.com/login/oauth/access_token"
            [ "client_id" := clientId
            , "client_secret" := clientSecret
            , "code" := code
            , "redirect_uri" := url <> "/private"
            ])
    >>= dec "Invalid auth response"
  let scopes = T.splitOn "," $ scope auth
  -- Check access rights
  unless ("user" `elem` scopes) $ fail "Unsufficient access rights"
  -- Get user emails
  (mails :: [EmailResponse]) <-
    liftIO (getWith (defaults & param "access_token" .~ [accessToken]) "https://api.github.com/user/emails")
    >>= dec "Invalid emails response"
  -- Show them
  defaultLayout [whamlet| Your emails: #{show mails} |]

main :: IO ()
main = do
  let genv = liftM T.pack . getEnv
  clientId <- genv "CLIENT_ID"
  clientSecret <- genv "CLIENT_SECRET"
  url <- genv "APP_URL"
  -- Start server with the environment.
  warp 3000 Project { .. }
