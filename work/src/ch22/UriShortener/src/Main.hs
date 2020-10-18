{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty.Trans

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs !! randomDigit)

shortyGen :: IO [Char]
shortyGen = replicateM 7 (randomElement alphaNum)

saveURI :: BC.ByteString
        -> BC.ByteString
        -> ReaderT R.Connection IO (Either R.Reply R.Status)
saveURI shortURI uri = do
  conn <- ask
  lift ( R.runRedis conn $ R.set shortURI uri)

getURI :: BC.ByteString
       -> ReaderT R.Connection IO (Either R.Reply (Maybe BC.ByteString))
getURI shortURI = do
  conn <- ask
  lift (R.runRedis conn $ R.get shortURI)

linkShorty :: String -> String
linkShorty shorty =
  concat
  [ "<a href=\""
  , shorty
  , "\">Copy and paste your short URL ( "
  , shorty
  , " )</a>"
  ]

shortyCreated :: Show a
              => a
              -> String
              -> TL.Text
shortyCreated resp shawty =
  TL.concat
  [ TL.pack (show resp)
  , " shorty is: "
  , TL.pack (linkShorty shawty)]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat
  [ uri
  , " wasn't a url."
  , " did you forgot http://?"
  ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat
  [ "<a href=\""
  , tbs, "\">"
  , tbs, "</a>"
  ]

type App a = ScottyT TL.Text (ReaderT R.Connection IO) a
type Action a = ActionT TL.Text (ReaderT R.Connection IO) a

app :: App ()
app = do
  get "/" $ getHome
  get "/:short" $ getShort

getHome :: Action ()
getHome = do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Just _ -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
            uri' = encodeUtf8 (TL.toStrict uri)
        existing <- lift (getURI shorty)
        case existing of
          Left reply -> text (TL.pack (show reply))
          Right mbBS -> case mbBS of
            Nothing -> do
              resp <- lift (saveURI shorty uri')
              html (shortyCreated resp shawty)
            Just _ -> text "double"
      Nothing -> text (shortyAintUri uri)

getShort :: Action ()
getShort = do
    short <- param "short"
    uri <- lift (getURI short)
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
  scottyT 3000 (flip runReaderT rConn) app

