module Client where

import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Simple
import Network.HTTP.Client.Conduit

httpSendMessage :: String -> String -> String -> IO Int
httpSendMessage message player gameId = do
    let request
            = setRequestMethod (BS.pack "POST")
            $ setRequestHeader (CI.mk $ BS.pack "Content-Type") [BS.pack "application/relaxed-bencoding"]
            $ setRequestBody (RequestBodyBS $ BS.pack message)
            $ httpBaseRequest player gameId
    response <- httpBS request
    -- putStrLn("sendMessage: " ++ gameId ++ "/" ++ player ++ " " ++ message)
    let code = getResponseStatusCode response
    -- putStrLn("answer:" ++ (BS.unpack $ getResponseBody response))
    return code

httpGetMessage :: String -> String -> IO (Int, String)
httpGetMessage player gameId = do
    let request
            = setRequestMethod (BS.pack "GET")
            $ setRequestHeader (CI.mk $ BS.pack "Accept") [BS.pack "application/relaxed-bencoding"]
            $ httpBaseRequest player gameId
    response <- httpBS request
    let code = getResponseStatusCode response
    -- putStrLn $ BS.unpack $ getResponseBody response
    return (code, BS.unpack $ getResponseBody response)

httpBaseRequest :: String -> String -> Request
httpBaseRequest player gameId
    = setRequestPath (BS.pack $ "/game/" ++ gameId ++ "/player/" ++ player)
    $ setRequestHost (BS.pack url)
    $ defaultRequest
    where
        url = "battleship.haskell.lt"