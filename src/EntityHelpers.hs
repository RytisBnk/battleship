module EntityHelpers where

import Entities

emptyMessage :: Message
emptyMessage = Message ("0", "0") "" NULL

getMessageList :: Message -> [Message] -- Messages are returned in reverse order (last to first)
getMessageList message = getMessageList' message []
    where 
        getMessageList' :: Message -> [Message] -> [Message]
        getMessageList' (Message coord result NULL) msgList = [Message coord result NULL] ++ msgList
        getMessageList' (Message coord result next) msgList = getMessageList' next ([Message coord result NULL] ++ msgList)

getMessageObject :: [Message] -> Message
getMessageObject messages = getMessageObject' messages NULL 
    where
        getMessageObject' :: [Message] -> Message -> Message
        getMessageObject' [] message = message
        getMessageObject' (first:rest) NULL = getMessageObject' rest first
        getMessageObject' (first:rest) message = getMessageObject' rest (appendNewMessage message first)

appendNewMessage :: Message -> Message -> Message -- target message, appended message, result
appendNewMessage NULL appended = appended
appendNewMessage (Message coord result NULL) appended = Message coord result appended
appendNewMessage (Message coord result next) appended = Message coord result $ appendNewMessage next appended

reverseMessage :: Message -> Message 
reverseMessage message = getMessageObject $ getMessageList message

getLastMessage :: Message -> Message
getLastMessage NULL = NULL
getLastMessage (Message coord result NULL) = Message coord result NULL
getLastMessage message = getLastMessage $ (next message)