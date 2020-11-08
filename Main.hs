{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Prelude hiding (catch)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.String
import Data.Maybe
import Data.ConfigFile
import Network.Mail.Mime
import Network.FastCGI -- from direct-fastcgi
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
import qualified Data.Map as Map


plaintextMail :: Address -- ^ to
              -> Address -- ^ from
              -> Text    -- ^ subject
              -> Text    -- ^ plain body
              -> Mail
plaintextMail to fromAddress subject plainBody = (emptyMail fromAddress) {
    mailTo = [to]
  , mailHeaders = [ ("Subject", subject) ]
  , mailParts = [[
         Part "text/plain; charset=utf-8" QuotedPrintableText Nothing []
           $ fromChunks [T.encodeUtf8 plainBody]
      ]]
  }


_RECIPIENT = Address { addressName = Just "Niklas Hambüchen",
                      addressEmail = "niklas@nh2.me" }

_SENDER = Address { addressName = Just "tellme",
                   addressEmail = "tellme@nh2.me" }

_SUBJECT = "tellme"
_DONE_REDIRECT = "/done.html"

sendMessage msg = renderSendMail $ plaintextMail _RECIPIENT _SENDER _SUBJECT msg

fPutStrLn s = fPutStr (s ++ "\n")

die code message = setResponseStatus code >> fPutStrLn message >> fCloseOutput

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll a handler = catch a (\e -> let _ = (e :: SomeException) in handler e)

handleAll = flip catchAll

handleRequest = do
  --user <- liftIO getEffectiveUserName
  --fPutStrLn $ "user: " ++ user
  setResponseHeader HttpContentType "text/html; charset=UTF-8"

  contents <- fGetContents
  _GET <- maybe Map.empty (parseUrlEncoded . fromString) <$> getQueryString
  let _POST = parseUrlEncoded contents

  let msgParam1 = listToMaybe <=< Map.lookup "msg" -- 1st passed msg param
                  -- prefer POST over GET over raw contents
      msg       = msgParam1 _POST <|> msgParam1 _GET <|> Just contents

  case msg of
    Just m
      | BS.length m > 1000000 -> die 413 "message too long"
      | BS.length m == 0      -> die 413 "no message entered"
      | otherwise             -> let text = T.decodeUtf8 m in
          do
            -- Send SMS
            smsSent <- liftIO $ sendSms (T.unpack text)

            let smsInfo = "\n\n--\n" ++ "SMS info: " ++ smsStatus smsSent

            -- Send Email
            emailsSent <- liftIO $ try (sendMessage $ text `T.append` T.pack smsInfo)

            case emailsSent of
              Left (SomeException _) -> die 500 "sending mail failed"
              Right _                -> do
                                          seeOtherRedirect _DONE_REDIRECT
                                          fPutStrLn "done"
    _      -> die 400 ("ERROR! GET: " ++ show _POST ++ " POST: " ++ show _POST)


main :: IO ()
main = acceptLoop forkIO handleRequest

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


failOnLeftException :: Monad m => Either SomeException (m a) -> m a
failOnLeftException e = case e of
  Left (SomeException ex) -> fail (show ex)
  Right x                 -> x

parseUriOrFail url = maybe (fail $ "malformed URI: " ++ url) return (parseURI url)

instance Get_C URI where
  get cp s o = do
    val <- get cp s o
    case parseURI val of
      Just x -> return x
      Nothing -> throwError (ParseError $ "couldn't parse URI " ++ val ++ " from "
                                          ++ "(" ++ s ++ "/" ++ o ++ ")",
                             "parseURI")


getSmsConfig :: IO (Either CPError SmsConfig)
getSmsConfig = do
  -- TODO configure server to see home directory
  --configPath <- (</> "config") <$> getAppUserDataDirectory "tellme"
  --let configPath = "/home/niklas/.tellme/config"
  let configPath = "/var/www/nh2.me/httpdocs/fcgi/config"
  runErrorT $ do
    cp <- join . liftIO $ readfile emptyCP configPath
    let conf = get cp "SMS" -- needs NoMonomorphismRestriction or duplication
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
