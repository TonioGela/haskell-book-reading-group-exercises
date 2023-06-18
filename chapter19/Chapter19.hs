{-# LANGUAGE OverloadedStrings #-}
module Chapter19 () where



import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Control.Monad.Reader
import Web.Scotty



alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

shortyGen :: IO [Char]
shortyGen = replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection
  -> BC.ByteString
  -> BC.ByteString
  -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

getURI :: R.Connection
  -> BC.ByteString
  -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI =
  R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL</a>"
         ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , TL.pack " shorty is: "
            , TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [uri
            , TL.pack " wasn't a url, did you forget http://?"
            ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat [TL.pack "<a href=\""
            , tbs
            , TL.pack   "\">"
            , tbs
            , TL.pack "</a>"]

newtype Config = Config { redisConnection :: R.Connection}

router :: Config -> ScottyM ()
router config = do
  get "/healtCheck" $ text "I'm alive!"
  get "/" $ saveUriHandler . redisConnection $ config
  get "/:short" $ getUriHandler . redisConnection $ config


saveUriHandler :: R.Connection -> ActionM ()
saveUriHandler rConn = do
  uri <- param "uri"
  let parsedUri :: Maybe URI
      parsedUri = parseURI (TL.unpack uri)
  case parsedUri of
    Just _ -> do
      shawty <- liftIO shortyGen
      let shorty = BC.pack shawty
          uri' = encodeUtf8 (TL.toStrict uri)
      resp <- liftIO (saveURI rConn shorty uri')
      html (shortyCreated resp shawty)
    Nothing -> text (shortyAintUri uri)


getUriHandler :: R.Connection -> ActionM ()
getUriHandler rConn = do
  short <- param "short"
  uri <- liftIO (getURI rConn short)
  case uri of
    Left reply -> text (TL.pack (show reply))
    Right mbBS -> case mbBS of
      Nothing -> text "uri not found"
      Just bs -> html (shortyFound tbs)
        where tbs :: TL.Text
              tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (router (Config rConn))
