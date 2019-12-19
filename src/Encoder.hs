module Encoder where

import Entities
import EntityHelpers

encodeMessage :: Message -> String
encodeMessage message = encodeMessageList $ getMessageList message

encodeMessageList :: [Message] -> String
encodeMessageList msgList = encodeMessageList' msgList "" 0
    where 
        encodeMessageList' :: [Message] -> String -> Int -> String
        encodeMessageList' [] message level = message ++ replicate (level + 1) 'e'
        encodeMessageList' (m:ms) message level = 
            let
                coords = coord m
                coordX = fst coords
                coordY = snd coords
                coordBencode = if (coordX == "0" && coordY == "0") 
                    then "d5:coordl" 
                    else "d5:coordl1:" ++ coordX ++ show (length coordY) ++ ":" ++ coordY
                resultBencode = if (result m == "") 
                    then "e"
                    else "e6:result" ++ show (length $ result m) ++ ":" ++ result m
                prevBencode = if (length ms > 0)
                    then "4:prev"
                    else ""
                appendedMessage = message ++ coordBencode ++ resultBencode ++ prevBencode
            in
                encodeMessageList' ms appendedMessage (level + 1)
