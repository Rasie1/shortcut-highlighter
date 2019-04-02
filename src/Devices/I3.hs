{-# LANGUAGE OverloadedStrings #-}
module Devices.I3 (
      connectI3
    , command
    , getWorkspaces
    , getWorkspacesConfig
    , getOutputs
    , getTree
    , getMarks
    , getBarConfig
    , Message
    , MessagePayload
    , WorkspaceConfig (..)
    ) where

import System.IO (hPutStr, hClose)
import System.Process.Typed
import qualified Control.Exception as Exception
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Binary as Binary
import qualified Data.Aeson as JSON
import Data.Binary.Get
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.Int
import Data.Aeson
import Data.HashMap.Strict ((!))
import qualified Data.Vector as Vector
import Data.Scientific (floatingOrInteger)
import Data.List.Index

data Workspace = Workspace { _urgent :: Bool, _visible :: Bool, _number :: Int }
    deriving (Eq, Show)


getWorkspacesConfig :: Socket -> IO [WorkspaceConfig]
getWorkspacesConfig socket = do
    workspaces <- getWorkspaces socket
    case workspaces of
        Right (Array a) -> do xs <- mapM getWorkspace a
                              return $ foldl convertWorkspace (replicate 14 WorkspaceEmpty) xs
        _ -> return []

convertWorkspace xs Workspace { _urgent = urgent, _visible = active, _number = i } = setAt (i - 1) new xs
    where new = case (urgent, active) of
                        (_, True) -> WorkspaceActive
                        (True, _) -> WorkspaceUrgent 
                        _ -> WorkspaceWindow

getWorkspace :: Value -> IO Workspace
getWorkspace s = case s of Object o -> case (o ! "urgent", o ! "visible", o ! "num") of
                            (Bool urgent, Bool active, Number num) -> do
                                case floatingOrInteger num of
                                    Right n -> return (Workspace urgent active n)

data WorkspaceConfig = WorkspaceEmpty 
                     | WorkspaceWindow 
                     | WorkspaceUrgent 
                     | WorkspaceActive
    deriving (Show)

type MessagePayload = String
data Message = Command MessagePayload
             | GetWorkspaces
             | Subscribe
             | GetOutputs
             | GetTree
             | GetMarks
             | GetBarConfig

encodeMessage :: Message -> U.ByteString
encodeMessage msg = U.fromString messageString
    where messageString :: String
          messageString = header <> messageLength <> messageType <> payload
          messageLength :: String
          messageLength = case msg of
            Command payload -> reverse . L8.unpack . Binary.encode $ (fromIntegral (length payload) :: Int32)
            otherwise -> "\x00\x00\x00\x00"
          messageType :: String
          messageType = case msg of
            (Command _)   -> "\x00\x00\x00\x00"
            GetWorkspaces -> "\x01\x00\x00\x00"
            Subscribe     -> "\x02\x00\x00\x00"
            GetOutputs    -> "\x03\x00\x00\x00"
            GetTree       -> "\x04\x00\x00\x00"
            GetMarks      -> "\x05\x00\x00\x00"
            GetBarConfig  -> "\x06\x00\x00\x00"
          header = "i3-ipc"
          payload :: String
          payload = case msg of 
            Command p -> p
            otherwise -> ""

deserializeHeader :: Get (Integer, Integer, L8.ByteString)
deserializeHeader = do
    header <- getLazyByteString 6
    if header == "i3-ipc" then do
        i1 <- getInt32le
        i2 <- getInt32le
        payload <- getRemainingLazyByteString
        return (fromIntegral i1, fromIntegral i2, payload)
    else fail "wrong header"

decodeMessage :: U.ByteString -> L8.ByteString
decodeMessage input =
    let (messageLength, messageType, payload) = runGet deserializeHeader (L8.pack $ U.toString input)
     in payload


getSocketPath :: IO String
getSocketPath = do
    (out, err) <- readProcess_ "i3 --get-socketpath"
    let unpacked = L8.unpack out
    let ret = take (length unpacked - 1) unpacked
    return ret

command :: Socket -> String -> IO (Either String JSON.Value)
command sock msg = getFromI3 sock (Command msg)

getWorkspaces :: Socket -> IO (Either String JSON.Value)
getWorkspaces sock = getFromI3 sock GetWorkspaces

getOutputs :: Socket -> IO (Either String JSON.Value)
getOutputs sock = getFromI3 sock GetOutputs

getTree :: Socket -> IO (Either String JSON.Value)
getTree sock = getFromI3 sock GetTree

getMarks :: Socket -> IO (Either String JSON.Value)
getMarks sock = getFromI3 sock GetMarks

getBarConfig :: Socket -> IO (Either String JSON.Value)
getBarConfig sock = getFromI3 sock GetBarConfig

i3msg :: Socket -> Message -> IO L8.ByteString
i3msg sock inputMessage = withSocketsDo $ do
    let messageToSend = encodeMessage inputMessage
    sendAll sock messageToSend
    msg <- recv sock 8192
    let payload = decodeMessage msg
    return payload

getFromI3 :: Socket -> Message -> IO (Either String JSON.Value)
getFromI3 sock inputMessage = do
    response <- i3msg sock inputMessage
    return (JSON.eitherDecode response :: Either String JSON.Value)

connectI3 :: IO Socket 
connectI3 = do
    path <- getSocketPath
    sock <- socket AF_UNIX Stream defaultProtocol
    connect sock $ SockAddrUnix path
    return sock