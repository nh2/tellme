{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Prelude hiding (catch)
import Data.Text (Text)
import Data.Maybe (listToMaybe)
import Data.Traversable (for)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import System.IO
import Data.String
import Data.Maybe
import qualified Data.ConfigFile as CF
import Network.Mail.Mime
import Network.HTTP
import Network.URI
import Network.Browser (Form (..), formToRequest)
import Control.Monad.Error
import Control.Concurrent (forkIO)
--import System.Posix.User
import Control.Applicative
import Snap.Internal.Parsing
import System.FilePath
import System.Directory
import Control.Exception
import qualified Data.ByteString as BS (length)
import Data.ByteString.Lazy (fromChunks)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map


import System.Posix.Files
import Data.Default (def)
import Network.Socket
import Network.HTTP.Types.Status
import Web.Scotty
import System.Environment (lookupEnv)


plaintextMail :: Address -- ^ to
              -> Address -- ^ from
              -> Text    -- ^ subject
              -> Text    -- ^ plain body
              -> Mail
plaintextMail to fromAddress subject plainBody = (emptyMail fromAddress) {
    mailTo = [to]
  , mailHeaders = [ ("Subject", subject) ]
  , mailParts = [[
      Part
        { partType = "text/plain; charset=utf-8"
        , partEncoding = QuotedPrintableText
        , partDisposition = DefaultDisposition
        , partHeaders = []
        , partContent = PartContent $ fromChunks [T.encodeUtf8 plainBody]
        }
    ]]
  }


_RECIPIENT = Address { addressName = Just "Niklas Hambüchen",
                      addressEmail = "niklas@nh2.me" }

_SENDER = Address { addressName = Just "tellme",
                   addressEmail = "tellme@nh2.me" }

_SUBJECT = "tellme"
_DONE_REDIRECT = "/done.html"

sendMessage msg = renderSendMail $ plaintextMail _RECIPIENT _SENDER _SUBJECT msg

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll a handler = catch a (\e -> let _ = (e :: SomeException) in handler e)

handleAll = flip catchAll

handleRequest :: ActionM ()
handleRequest = do
  msg :: Text <- param "msg"

  if
    | T.length msg > 1000000 -> raiseStatus requestEntityTooLarge413 "message too long"
    | T.length msg == 0      -> raiseStatus badRequest400            "no message entered"
    | otherwise -> do
        -- Send SMS
        smsSent <- liftAndCatchIO $ sendSms (T.unpack msg)

        let smsInfo = "\n\n--\n" ++ "SMS info: " ++ smsStatus smsSent

        -- Send Email
        emailsSent <- liftAndCatchIO $ try (sendMessage $ msg `T.append` T.pack smsInfo)

        case emailsSent of
          Left (e :: SomeException) -> do
            liftAndCatchIO $ T.hPutStrLn stderr ("Error sending email: " <> T.pack (show e))
            raiseStatus internalServerError500 "sending mail failed"
          Right{} -> do
            redirect _DONE_REDIRECT
            text "done"


main :: IO ()
main = do
  let runtimeDirEnvVars =
        [ "RUNTIME_DIRECTORY"
        , "XDG_RUNTIME_DIR"
        , "TMPDIR"
        ]
  mbRuntimeDir <- listToMaybe . catMaybes <$> for runtimeDirEnvVars lookupEnv
  socketFile <- case mbRuntimeDir of
    Nothing -> fail $ "Please set one of the " ++ show runtimeDirEnvVars ++ " environment variable for the socket file"
    Just runtimeDir -> do
      let socketFile = runtimeDir </> "tellme.socket"
      createDirectoryIfMissing True (takeDirectory socketFile)
      pure socketFile
  removePathForcibly socketFile -- clean up

  -- Open the socket
  sock <- socket AF_UNIX Stream 0
  bind sock $ SockAddrUnix socketFile
  setFileMode socketFile stdFileMode -- ensure `chmod 666` so nginx can write to us
  listen sock maxListenQueue

  let options = def -- I don't like `data-default`, at least give it a name here

  scottySocket options sock $ do
    post "/" handleRequest


test = sendSms "asdf" >>= putStrLn . smsStatus

data SmsConfig = SmsConfig {
    enabled :: Bool
  , username :: String
  , password :: String
  , phone :: String

  , post_uri :: URI
  , user_param :: String
  , password_param :: String
  , message_param :: String
  , phone_number_param :: String
} deriving (Eq, Show)


failOnLeftException :: MonadFail m => Either SomeException (m a) -> m a
failOnLeftException e = case e of
  Left (SomeException ex) -> fail (show ex)
  Right x                 -> x

parseUriOrFail url = maybe (fail $ "malformed URI: " ++ url) return (parseURI url)

instance CF.Get_C URI where
  get cp s o = do
    val <- CF.get cp s o
    case parseURI val of
      Just x -> return x
      Nothing -> throwError (CF.ParseError $ "couldn't parse URI " ++ val ++ " from "
                                          ++ "(" ++ s ++ "/" ++ o ++ ")",
                             "parseURI")


getSmsConfig :: IO (Either CF.CPError SmsConfig)
getSmsConfig = do
  configPath <- (</> "config") <$> getXdgDirectory XdgConfig "tellme"
  runErrorT $ do
    cp <- join . liftIO $ CF.readfile CF.emptyCP configPath
    let conf = CF.get cp "SMS" -- needs NoMonomorphismRestriction or duplication
    SmsConfig <$> conf "enabled"
              <*> conf "user"
              <*> conf "password"
              <*> conf "phone"
              <*> conf "post_uri"
              <*> conf "user_param"
              <*> conf "password_param"
              <*> conf "message_param"
              <*> conf "phone_number_param"


postFormRequest uri formData = formToRequest $ Form POST uri formData


smsTrim s
  | length s < 160 = s
  | otherwise      = take 154 s ++ " [...]"

sendSms :: String -> IO (Either String (Maybe String))
sendSms fullMsg = do
  let trimmedMsg = smsTrim fullMsg
  handleAll (failShow "EXCEPTION LOADING CONFIG FILE") $ do
    eSmsConf <- getSmsConfig
    case eSmsConf of
      Left e -> failShow "CONFIG FILE PARSING ERROR" e
      Right (SmsConfig {
               enabled,
               username, password, phone,
               post_uri,
               user_param, password_param, message_param, phone_number_param
             })
        | enabled -> let req = postFormRequest post_uri [
                                   (user_param, username)
                                 , (password_param, password)
                                 , (phone_number_param, phone)
                                 , (message_param, trimmedMsg)
                               ]
                      in handleAll (failShow "EXCEPTION ON SMS SERVICE") $ do
                                      res <- simpleHTTP req >>= getResponseBody
                                      return . Right . Just $ res

        | otherwise -> return . Right $ Nothing
  where
    failShow msg toShow = return . Left $ msg ++ ": " ++ show toShow

-- Turns the result of 'sendSms' into a String.
smsStatus smsResult = case smsResult of
  Left err             -> "Error: " ++ err
  Right (Just service) -> "SMS service response: " ++ service
  Right _              -> "SMS notification disabled"
