module BencodeParser where

import Entities
import EntityHelpers

import Data.Char
import Data.List
import Data.Either
import Debug.Trace

parseBencode :: String -> Either String (Message, String)
parseBencode [] = Right (NULL, "")
parseBencode message = 
    case parseBencode' message (Message ("0", "0") "" NULL) of
        Left error -> Left error
        Right (messageObj, restOfBencode) -> Right (reverseMessage messageObj, restOfBencode)
    where 
        parseBencode' :: String -> Message -> Either String (Message, String)
        parseBencode' ('d':message) messageObj = 
            parseBencode' message messageObj
        parseBencode' ('e':message) messageObj = Right (messageObj, message)
        parseBencode' message (Message inCoord inResult next) = 
            case parseByteString message of 
                Left error -> Left error
                Right (key, restOfMessage) ->
                    case key of
                        "result" ->
                            case parseResult restOfMessage of
                                Left error -> Left error
                                Right (result, remainingMessage) -> 
                                    parseBencode' remainingMessage (Message inCoord result next)
                        "coord" -> 
                            case parseCoord restOfMessage of
                                Left error -> Left error
                                Right (coord, remainingMessage) ->
                                    parseBencode' remainingMessage (Message coord inResult next)
                        "prev" -> 
                            case parseBencode' restOfMessage emptyMessage of
                                Left error -> Left error
                                Right (messageObj, remainder) -> 
                                    parseBencode' remainder (Message inCoord inResult messageObj)
                        _ -> Left ("Invalid bencode, key is not accapted: " ++ key)

parseByteString :: String -> Either String (String, String)
parseByteString [] = Right ([],[])
parseByteString (x:xs) | isDigit x = 
    let
        strLenStr = [x] ++ takeWhile isDigit xs
        strLen = read strLenStr
        lenOfLen = length strLenStr
        takeLen = lenOfLen + strLen
        str = take takeLen xs
        in 
            if (head str) == ':' 
                then Right (tail str, drop takeLen xs)
            else Left "Empty byte string"
parseByteString message = Left ("Invalid encoding for a byte string: " ++ message)

parseResult :: String -> Either String (String, String)
parseResult [] = Left "Invalid format. Result field cannot be empty"
parseResult ('3':':':'H':'I':'T':rest) = Right ("HIT", rest)
parseResult ('4':':':'M':'I':'S':'S':rest) = Right ("MISS", rest)
parseResult _ = Left "Invalid format. Result must be one of the specified values: {HIT, MISS}"

parseCoord :: String -> Either String ((String, String), String)
parseCoord [] = Left "Invalid format. List cannot be empty"
parseCoord ('l':'e':message) = Right (("0", "0"), message)
parseCoord ('l':message) =
    let
        Right (elements, rest) = parseList ("l" ++ message)
        elementCount = length elements
    in 
        if elementCount == 0 
            then Right (("0", "0"), rest)
        else if elementCount == 2 
            then Right ((elements !! 0, elements !! 1), rest)
        else Left "Invalid format. Field 'coord' must have exactly 2 elements"

parseList :: String -> Either String ([String], String)
parseList ('l':xs) = parseList' xs []
    where
        parseList' :: String -> [String] -> Either String ([String], String)
        parseList' ('e':xs) r = Right (r, xs)
        parseList' xs r = 
            case parseByteString xs of
                Left e -> Left e
                Right (res, rest) -> parseList' rest (r ++ [res])

--Testai

message = fst (fromRight (NULL, "") (parseBencode "d4:prevd5:coordl1:A2:10ee6:result4:MISS5:coordl1:K1:7ee"))
msgList = getMessageList message
msgObj = getMessageObject msgList
message2 = Message ("C", "6") "MISS" message