module Game where

import BencodeParser
import Entities
import EntityHelpers
import Encoder 
import Helpers
import Client

import Data.Either
import Data.Tuple
import Debug.Trace

board :: [Cell]
board = [("C", "5"), ("D", "5"), ("E", "5"), ("F", "5"),
        ("B", "8"), ("C", "8"), ("D", "8"),   ("I", "2"), ("I", "3"), ("I", "4"),
        ("B", "1"), ("B", "2"),   ("C", "10"), ("D", "10"),   ("J", "7"), ("J", "8"),
        ("A", "6"),   ("D", "3"),   ("F", "2"),   ("H", "10")]

getPlayerTargets :: Message -> ([Cell], [Cell])
getPlayerTargets message = getPlayerTargets' message 0 ([], [])
    where
        getPlayerTargets' :: Message -> Int -> ([Cell], [Cell]) -> ([Cell], [Cell])
        getPlayerTargets' NULL msgCount targets = targets
        getPlayerTargets' (Message coord result next) msgCount (targetsA, targetsB) =
            if msgCount `mod` 2 == 0 then
                getPlayerTargets' next (msgCount + 1) (targetsA ++ [coord], targetsB)
            else 
                getPlayerTargets' next (msgCount + 1) (targetsA, targetsB ++ [coord])

getOpponentShotResult :: Message -> String 
getOpponentShotResult message = 
    let 
        lastMessage = getLastMessage message 
    in
        if (coord lastMessage) `elem` board then "HIT"
        else "MISS"

addMoveToMessage :: Message -> Cell -> Message
addMoveToMessage NULL coord = Message coord "" NULL
addMoveToMessage message coord = 
    let
        result = getOpponentShotResult message
        newMessage = Message coord result NULL
    in
        appendNewMessage message newMessage

gameLoop :: String -> String -> IO()
gameLoop gameId player = do
    case player of
        "A" -> 
            startPlayerA gameId player
        "B" ->
            respondToMove gameId player

startPlayerA :: String -> String -> IO()
startPlayerA gameId player = do
    firstMessage <- addNextShotMessage NULL player
    let encodedMessage = encodeMessage firstMessage
    statusCode <- httpSendMessage encodedMessage player gameId
    if statusCode == 204 then do
        putStrLn $ show statusCode
        respondToMove gameId player
    else if statusCode == 409 then do
        putStrLn $ show statusCode
        putStrLn "A game with the same id was probably already finished, choose a new id"
    else 
        putStrLn "Unknown error or some shit"

respondToMove :: String -> String -> IO()
respondToMove gameId player = do
    messageStr <- getOpponentMove gameId player 20
    if messageStr == "" then do
        putStrLn "Enemy has not responded for too long"
    else 
        case parseBencode messageStr of
            Left error -> 
                putStrLn error
            Right (message, rest) -> do
                case checkEndCondition message player of
                    Left error ->
                        putStrLn error
                    Right (endConditionReached, winner) -> 
                        if endConditionReached then do
                            let responseMessage = addMoveToMessage message ("0", "0")
                            let encodedMessage = encodeMessage responseMessage
                            if winner == player then do
                                putStrLn "You win bruh"
                            else do
                                statusCode <- httpSendMessage encodedMessage player gameId
                                putStrLn $ show statusCode
                                putStrLn (encodedMessage ++ "\n")
                                putStrLn ("You lost lmao, get rekt" ++ "\n")
                        else do
                            responseMessage <- addNextShotMessage message player
                            let encodedMessage = encodeMessage responseMessage
                            statusCode <- httpSendMessage encodedMessage player gameId
                            putStrLn $ show statusCode
                            respondToMove gameId player

addNextShotMessage :: Message -> String -> IO(Message)
addNextShotMessage message player = do
    let playerTargets = getPlayerTargets message
    randomCoord <- getRandomCoord
    let enemyTargets = case player of
            "A" -> fst playerTargets
            "B" -> snd playerTargets
    if randomCoord `elem` enemyTargets 
        then addNextShotMessage message player
    else 
        return (addMoveToMessage message randomCoord)
    
getOpponentMove :: String -> String -> Int -> IO(String)
getOpponentMove gameId player attemptsLeft = do
    (responseCode, responseBody) <- httpGetMessage player gameId
    if responseCode == 200
        then 
            return $ (responseBody)
    else if responseCode == 409 && attemptsLeft > 0
        then do
            putStrLn ("No move available, trying again. Retries left: " ++ show attemptsLeft)
            getOpponentMove gameId player (attemptsLeft - 1)
    else return $ ("")

checkEndCondition :: Message -> String -> Either String (Bool, String)
checkEndCondition message player = 
    case countScore message of
        Left error -> Left error
        Right (hitsA, hitsB) ->
            let
                previousResult = getOpponentShotResult message
                (scoreA, scoreB) = if previousResult == "MISS" then (hitsA, hitsB)
                    else case player of
                        "A" -> (hitsA, hitsB + 1)
                        "B" -> (hitsA + 1, hitsB)
            in trace (show (scoreA, scoreB)) $
                if scoreA == 20 then Right (True, "A")
                else if scoreB == 20 then Right (True, "B")
                else Right (False, "")


countScore :: Message -> Either String (Int, Int)
countScore message = 
    if isMessageValid message 
        then countScore' message 0 0 0
    else Left ("Only one move can be preformed for each individual cell" ++ show message)
    where 
        countScore' :: Message -> Int -> Int -> Int -> Either String (Int, Int)  -- message, number of prior moves, hits A, hits B
        countScore' NULL _ hitsA hitsB = Right (hitsA, hitsB)
        countScore' message count hitsA hitsB = 
            case result message of
                "HIT" ->
                    case mod count 2 of
                        0 -> countScore' (Entities.next message) (count + 1) hitsA (hitsB + 1)
                        1 -> countScore' (Entities.next message) (count + 1) (hitsA + 1) hitsB
                "MISS" -> countScore' (Entities.next message) (count + 1) hitsA hitsB
                "" -> 
                    if count == 0 then
                        countScore' (Entities.next message) (count + 1) hitsA hitsB
                    else Left ("Result undefined for message that isn't first " ++ (show message))

isMessageValid :: Message -> Bool
isMessageValid message = validateMessage' message [] [] 0
    where
        validateMessage' :: Message -> [(String, String)] -> [(String, String)] -> Int -> Bool
        validateMessage' NULL _ _ _ = True
        validateMessage' message previousHitsA previousHitsB previousMoves = 
            case mod previousMoves 2 of 
                0 ->
                    if coord message `elem` previousHitsA 
                        then False
                        else validateMessage' (Entities.next message) (previousHitsA ++ [coord message]) previousHitsB (previousMoves + 1)
                1 -> 
                    if coord message `elem` previousHitsB 
                        then False
                        else validateMessage' (Entities.next message) previousHitsA (previousHitsB ++ [coord message]) (previousMoves + 1)


-- TESTAI
message = fst (fromRight (NULL, "") (parseBencode "d5:coordl1:C1:3e6:result3:HIT4:prevd6:result4:MISS4:prevd5:coordl1:A1:1ee5:coordl1:B1:3eee"))
newMessage = Message {coord = ("A", "7"), result = "MISS", Entities.next = NULL}

testas1 = fst $ fromRight (NULL, "") (parseBencode "d5:coordl1:A1:3ee")
testas2 = Message {coord = ("A","3"), result = "", next = Message {coord = ("I","2"), result = "MISS", next = NULL}}
testas3 = Message {coord = ("A","3"), result = "", next = Message {coord = ("I","2"), result = "MISS", next = Message {coord = ("E","5"), result = "HIT", next = NULL}}}

getPlayerHits :: Message -> ([Cell], [Cell])
getPlayerHits message = 
    let 
        (targetsA, targetsB) = getPlayerTargets message
        hitsA = filter (\x -> elem x board) targetsA
        hitsB = filter (\x -> elem x board) targetsB
    in
        (hitsA, hitsB)

--Message {coord = ("D","5"), result = "", next = Message {coord = ("E","4"), result = "HIT", next = Message {coord = ("C","9"), result = "MISS", next = Message {coord = ("G","5"), result = "MISS", next = Message {coord = ("C","6"), result = "MISS", next = Message {coord = ("H","5"), result = "MISS", next = Message {coord = ("B","5"), result = "MISS", next = Message {coord = ("G","10"), result = "MISS", next = Message {coord = ("F","4"), result = "MISS", next = Message {coord = ("H","9"), result = "MISS", next = Message {coord = ("E","8"), result = "MISS", next = Message {coord = ("J","10"), result = "MISS", next = Message {coord = ("F","3"), result = "MISS", next = Message {coord = ("D","3"), result = "MISS", next = Message {coord = ("B","2"), result = "HIT", next = Message {coord = ("H","2"), result = "HIT", next = Message {coord = ("G","6"), result = "MISS", next = Message {coord = ("I","2"), result = "MISS", next = Message {coord = ("H","3"), result = "HIT", next = Message {coord = ("I","4"), result = "MISS", next = Message {coord = ("F","6"), result = "HIT", next = Message {coord = ("F","3"), result = "MISS", next = Message {coord = ("A","2"), result = "MISS", next = Message {coord = ("B","4"), result = "MISS", next = Message {coord = ("G","9"), result = "MISS", next = Message {coord = ("A","9"), result = "MISS", next = Message {coord = ("G","7"), result = "MISS", next = Message {coord = ("I","8"), result = "MISS", next = Message {coord = ("B","7"), result = "MISS", next = Message {coord = ("F","1"), result = "MISS", next = Message {coord = ("E","3"), result = "MISS", next = Message {coord = ("D","1"), result = "MISS", next = Message {coord = ("C","10"), result = "MISS", next = Message {coord = ("C","2"), result = "HIT", next = Message {coord = ("J","7"), result = "MISS", next = Message {coord = ("E","9"), result = "HIT", next = Message {coord = ("A","8"), result = "MISS", next = Message {coord = ("G","1"), result = "MISS", next = Message {coord = ("E","5"), result = "MISS", next = Message {coord = ("I","5"), result = "HIT", next = Message {coord = ("H","1"), result = "MISS", next = Message {coord = ("I","7"), result = "MISS", next = Message {coord = ("I","3"), result = "MISS", next = Message {coord = ("C","7"), result = "HIT", next = Message {coord = ("F","5"), result = "MISS", next = Message {coord = ("D","7"), result = "HIT", next = Message {coord = ("G","10"), result = "MISS", next = Message {coord = ("C","6"), result = "MISS", next = Message {coord = ("J","10"), result = "MISS", next = Message {coord = ("J","1"), result = "MISS", next = Message {coord = ("A","1"), result = "MISS", next = Message {coord = ("E","7"), result = "MISS", next = Message {coord = ("E","7"), result = "MISS", next = Message {coord = ("E","1"), result = "MISS", next = Message {coord = ("A","3"), result = "MISS", next = Message {coord = ("F","8"), result = "MISS", next = Message {coord = ("I","10"), result = "MISS", next = Message {coord = ("E","5"), result = "MISS", next = Message {coord = ("G","5"), result = "HIT", next = Message {coord = ("D","10"), result = "MISS", next = Message {coord = ("F","7"), result = "HIT", next = Message {coord = ("J","6"), result = "MISS", next = Message {coord = ("C","7"), result = "MISS", next = Message {coord = ("E","6"), result = "MISS", next = Message {coord = ("A","6"), result = "MISS", next = Message {coord = ("C","8"), result = "HIT", next = Message {coord = ("B","1"), result = "HIT", next = Message {coord = ("H","8"), result = "HIT", next = Message {coord = ("D","1"), result = "MISS", next = Message {coord = ("A","5"), result = "MISS", next = Message {coord = ("G","2"), result = "MISS", next = Message {coord = ("G","6"), result = "MISS", next = Message {coord = ("D","10"), result = "MISS", next = Message {coord = ("H","3"), result = "HIT", next = Message {coord = ("D","6"), result = "MISS", next = Message {coord = ("E","10"), result = "MISS", next = Message {coord = ("G","3"), result = "MISS", next = Message {coord = ("D","9"), result = "MISS", next = Message {coord = ("H","8"), result = "MISS", next = Message {coord = ("C","10"), result = "MISS", next = Message {coord = ("H","6"), result = "HIT", next = Message {coord = ("J","9"), result = "MISS", next = Message {coord = ("J","9"), result = "MISS", next = Message {coord = ("B","10"), result = "MISS", next = Message {coord = ("D","9"), result = "MISS", next = Message {coord = ("B","6"), result = "MISS", next = Message {coord = ("E","10"), result = "MISS", next = Message {coord = ("C","1"), result = "MISS", next = Message {coord = ("C","8"), result = "MISS", next = Message {coord = ("G","2"), result = "HIT", next = Message {coord = ("J","8"), result = "MISS", next = Message {coord = ("H","6"), result = "HIT", next = Message {coord = ("E","2"), result = "MISS", next = Message {coord = ("B","8"), result = "MISS", next = Message {coord = ("I","1"), result = "HIT", next = Message {coord = ("G","7"), result = "MISS", next = Message {coord = ("B","10"), result = "MISS", next = Message {coord = ("E","2"), result = "MISS", next = Message {coord = ("B","3"), result = "MISS", next = Message {coord = ("B","5"), result = "MISS", next = Message {coord = ("D","2"), result = "MISS", next = Message {coord = ("D","6"), result = "MISS", next = Message {coord = ("J","2"), result = "MISS", next = Message {coord = ("F","10"), result = "MISS", next = Message {coord = ("F","8"), result = "MISS", next = Message {coord = ("B","1"), result = "MISS", next = Message {coord = ("G","4"), result = "HIT", next = Message {coord = ("F","7"), result = "MISS", next = Message {coord = ("A","10"), result = "MISS", next = Message {coord = ("G","4"), result = "MISS", next = Message {coord = ("B","9"), result = "MISS", next = Message {coord = ("F","4"), result = "MISS", next = Message {coord = ("E","9"), result = "MISS", next = Message {coord = ("F","5"), result = "MISS", next = Message {coord = ("H","9"), result = "HIT", next = Message {coord = ("D","8"), result = "MISS", next = Message {coord = ("C","3"), result = "HIT", next = Message {coord = ("G","9"), result = "MISS", next = Message {coord = ("I","9"), result = "MISS", next = Message {coord = ("B","2"), result = "MISS", next = Message {coord = ("J","6"), result = "HIT", next = Message {coord = ("H","4"), result = "MISS", next = Message {coord = ("D","4"), result = "MISS", next = Message {coord = ("C","5"), result = "MISS", next = Message {coord = ("F","2"), result = "HIT", next = Message {coord = ("A","8"), result = "HIT", next = Message {coord = ("G","1"), result = "MISS", next = Message {coord = ("H","10"), result = "MISS", next = Message {coord = ("H","10"), result = "HIT", next = Message {coord = ("I","6"), result = "HIT", next = Message {coord = ("I","4"), result = "MISS", next = Message {coord = ("F","9"), result = "HIT", next = Message {coord = ("F","10"), result = "MISS", next = Message {coord = ("A","4"), result = "MISS", next = Message {coord = ("B","8"), result = "MISS", next = Message {coord = ("D","4"), result = "HIT", next = Message {coord = ("C","2"), result = "MISS", next = Message {coord = ("A","2"), result = "MISS", next = Message {coord = ("F","9"), result = "MISS", next = Message {coord = ("C","9"), result = "MISS", next = Message {coord = ("I","7"), result = "MISS", next = Message {coord = ("A","3"), result = "MISS", next = Message {coord = ("H","5"), result = "MISS", next = Message {coord = ("I","9"), result = "MISS", next = Message {coord = ("E","4"), result = "MISS", next = Message {coord = ("A","7"), result = "MISS", next = Message {coord = ("J","4"), result = "MISS", next = Message {coord = ("B","9"), result = "MISS", next = Message {coord = ("A","7"), result = "MISS", next = Message {coord = ("I","3"), result = "MISS", next = Message {coord = ("I","5"), result = "HIT", next = Message {coord = ("J","4"), result = "MISS", next = Message {coord = ("C","4"), result = "MISS", next = Message {coord = ("B","3"), result = "MISS", next = Message {coord = ("C","5"), result = "MISS", next = Message {coord = ("E","8"), result = "HIT", next = Message {coord = ("D","8"), result = "MISS", next = Message {coord = ("J","3"), result = "HIT", next = Message {coord = ("H","2"), result = "MISS", next = Message {coord = ("G","3"), result = "MISS", next = Message {coord = ("F","1"), result = "MISS", next = Message {coord = ("I","1"), result = "MISS", next = Message {coord = ("I","2"), result = "MISS", next = Message {coord = ("D","5"), result = "HIT", next = Message {coord = ("G","8"), result = "HIT", next = Message {coord = ("H","1"), result = "MISS", next = Message {coord = ("C","1"), result = "MISS", next = Message {coord = ("F","2"), result = "MISS", next = Message {coord = ("J","3"), result = "HIT", next = Message {coord = ("H","7"), result = "MISS", next = Message {coord = ("A","5"), result = "MISS", next = Message {coord = ("C","3"), result = "MISS", next = Message {coord = ("B","4"), result = "MISS", next = Message {coord = ("I","10"), result = "MISS", next = Message {coord = ("A","9"), result = "MISS", next = Message {coord = ("B","7"), result = "MISS", next = Message {coord = ("E","1"), result = "MISS", next = Message {coord = ("E","3"), result = "MISS", next = Message {coord = ("B","6"), result = "MISS", next = Message {coord = ("J","5"), result = "MISS", next = Message {coord = ("J","5"), result = "MISS", next = Message {coord = ("J","7"), result = "MISS", next = Message {coord = ("H","4"), result = "HIT", next = Message {coord = ("F","6"), result = "MISS", next = Message {coord = ("I","8"), result = "MISS", next = Message {coord = ("A","6"), result = "MISS", next = Message {coord = ("J","1"), result = "HIT", next = Message {coord = ("J","2"), result = "MISS", next = NULL}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
-- "d5:coordl1:J1:2e6:result4:MISS4:prevd5:coordl1:J1:1e6:result3:HIT4:prevd5:coordl1:A1:6e6:result4:MISS4:prevd5:coordl1:I1:8e6:result4:MISS4:prevd5:coordl1:F1:6e6:result4:MISS4:prevd5:coordl1:H1:4e6:result3:HIT4:prevd5:coordl1:J1:7e6:result4:MISS4:prevd5:coordl1:J1:5e6:result4:MISS4:prevd5:coordl1:J1:5e6:result4:MISS4:prevd5:coordl1:B1:6e6:result4:MISS4:prevd5:coordl1:E1:3e6:result4:MISS4:prevd5:coordl1:E1:1e6:result4:MISS4:prevd5:coordl1:B1:7e6:result4:MISS4:prevd5:coordl1:A1:9e6:result4:MISS4:prevd5:coordl1:I2:10e6:result4:MISS4:prevd5:coordl1:B1:4e6:result4:MISS4:prevd5:coordl1:C1:3e6:result4:MISS4:prevd5:coordl1:A1:5e6:result4:MISS4:prevd5:coordl1:H1:7e6:result4:MISS4:prevd5:coordl1:J1:3e6:result3:HIT4:prevd5:coordl1:F1:2e6:result4:MISS4:prevd5:coordl1:C1:1e6:result4:MISS4:prevd5:coordl1:H1:1e6:result4:MISS4:prevd5:coordl1:G1:8e6:result3:HIT4:prevd5:coordl1:D1:5e6:result3:HIT4:prevd5:coordl1:I1:2e6:result4:MISS4:prevd5:coordl1:I1:1e6:result4:MISS4:prevd5:coordl1:F1:1e6:result4:MISS4:prevd5:coordl1:G1:3e6:result4:MISS4:prevd5:coordl1:H1:2e6:result4:MISS4:prevd5:coordl1:J1:3e6:result3:HIT4:prevd5:coordl1:D1:8e6:result4:MISS4:prevd5:coordl1:E1:8e6:result3:HIT4:prevd5:coordl1:C1:5e6:result4:MISS4:prevd5:coordl1:B1:3e6:result4:MISS4:prevd5:coordl1:C1:4e6:result4:MISS4:prevd5:coordl1:J1:4e6:result4:MISS4:prevd5:coordl1:I1:5e6:result3:HIT4:prevd5:coordl1:I1:3e6:result4:MISS4:prevd5:coordl1:A1:7e6:result4:MISS4:prevd5:coordl1:B1:9e6:result4:MISS4:prevd5:coordl1:J1:4e6:result4:MISS4:prevd5:coordl1:A1:7e6:result4:MISS4:prevd5:coordl1:E1:4e6:result4:MISS4:prevd5:coordl1:I1:9e6:result4:MISS4:prevd5:coordl1:H1:5e6:result4:MISS4:prevd5:coordl1:A1:3e6:result4:MISS4:prevd5:coordl1:I1:7e6:result4:MISS4:prevd5:coordl1:C1:9e6:result4:MISS4:prevd5:coordl1:F1:9e6:result4:MISS4:prevd5:coordl1:A1:2e6:result4:MISS4:prevd5:coordl1:C1:2e6:result4:MISS4:prevd5:coordl1:D1:4e6:result3:HIT4:prevd5:coordl1:B1:8e6:result4:MISS4:prevd5:coordl1:A1:4e6:result4:MISS4:prevd5:coordl1:F2:10e6:result4:MISS4:prevd5:coordl1:F1:9e6:result3:HIT4:prevd5:coordl1:I1:4e6:result4:MISS4:prevd5:coordl1:I1:6e6:result3:HIT4:prevd5:coordl1:H2:10e6:result3:HIT4:prevd5:coordl1:H2:10e6:result4:MISS4:prevd5:coordl1:G1:1e6:result4:MISS4:prevd5:coordl1:A1:8e6:result3:HIT4:prevd5:coordl1:F1:2e6:result3:HIT4:prevd5:coordl1:C1:5e6:result4:MISS4:prevd5:coordl1:D1:4e6:result4:MISS4:prevd5:coordl1:H1:4e6:result4:MISS4:prevd5:coordl1:J1:6e6:result3:HIT4:prevd5:coordl1:B1:2e6:result4:MISS4:prevd5:coordl1:I1:9e6:result4:MISS4:prevd5:coordl1:G1:9e6:result4:MISS4:prevd5:coordl1:C1:3e6:result3:HIT4:prevd5:coordl1:D1:8e6:result4:MISS4:prevd5:coordl1:H1:9e6:result3:HIT4:prevd5:coordl1:F1:5e6:result4:MISS4:prevd5:coordl1:E1:9e6:result4:MISS4:prevd5:coordl1:F1:4e6:result4:MISS4:prevd5:coordl1:B1:9e6:result4:MISS4:prevd5:coordl1:G1:4e6:result4:MISS4:prevd5:coordl1:A2:10e6:result4:MISS4:prevd5:coordl1:F1:7e6:result4:MISS4:prevd5:coordl1:G1:4e6:result3:HIT4:prevd5:coordl1:B1:1e6:result4:MISS4:prevd5:coordl1:F1:8e6:result4:MISS4:prevd5:coordl1:F2:10e6:result4:MISS4:prevd5:coordl1:J1:2e6:result4:MISS4:prevd5:coordl1:D1:6e6:result4:MISS4:prevd5:coordl1:D1:2e6:result4:MISS4:prevd5:coordl1:B1:5e6:result4:MISS4:prevd5:coordl1:B1:3e6:result4:MISS4:prevd5:coordl1:E1:2e6:result4:MISS4:prevd5:coordl1:B2:10e6:result4:MISS4:prevd5:coordl1:G1:7e6:result4:MISS4:prevd5:coordl1:I1:1e6:result3:HIT4:prevd5:coordl1:B1:8e6:result4:MISS4:prevd5:coordl1:E1:2e6:result4:MISS4:prevd5:coordl1:H1:6e6:result3:HIT4:prevd5:coordl1:J1:8e6:result4:MISS4:prevd5:coordl1:G1:2e6:result3:HIT4:prevd5:coordl1:C1:8e6:result4:MISS4:prevd5:coordl1:C1:1e6:result4:MISS4:prevd5:coordl1:E2:10e6:result4:MISS4:prevd5:coordl1:B1:6e6:result4:MISS4:prevd5:coordl1:D1:9e6:result4:MISS4:prevd5:coordl1:B2:10e6:result4:MISS4:prevd5:coordl1:J1:9e6:result4:MISS4:prevd5:coordl1:J1:9e6:result4:MISS4:prevd5:coordl1:H1:6e6:result3:HIT4:prevd5:coordl1:C2:10e6:result4:MISS4:prevd5:coordl1:H1:8e6:result4:MISS4:prevd5:coordl1:D1:9e6:result4:MISS4:prevd5:coordl1:G1:3e6:result4:MISS4:prevd5:coordl1:E2:10e6:result4:MISS4:prevd5:coordl1:D1:6e6:result4:MISS4:prevd5:coordl1:H1:3e6:result3:HIT4:prevd5:coordl1:D2:10e6:result4:MISS4:prevd5:coordl1:G1:6e6:result4:MISS4:prevd5:coordl1:G1:2e6:result4:MISS4:prevd5:coordl1:A1:5e6:result4:MISS4:prevd5:coordl1:D1:1e6:result4:MISS4:prevd5:coordl1:H1:8e6:result3:HIT4:prevd5:coordl1:B1:1e6:result3:HIT4:prevd5:coordl1:C1:8e6:result3:HIT4:prevd5:coordl1:A1:6e6:result4:MISS4:prevd5:coordl1:E1:6e6:result4:MISS4:prevd5:coordl1:C1:7e6:result4:MISS4:prevd5:coordl1:J1:6e6:result4:MISS4:prevd5:coordl1:F1:7e6:result3:HIT4:prevd5:coordl1:D2:10e6:result4:MISS4:prevd5:coordl1:G1:5e6:result3:HIT4:prevd5:coordl1:E1:5e6:result4:MISS4:prevd5:coordl1:I2:10e6:result4:MISS4:prevd5:coordl1:F1:8e6:result4:MISS4:prevd5:coordl1:A1:3e6:result4:MISS4:prevd5:coordl1:E1:1e6:result4:MISS4:prevd5:coordl1:E1:7e6:result4:MISS4:prevd5:coordl1:E1:7e6:result4:MISS4:prevd5:coordl1:A1:1e6:result4:MISS4:prevd5:coordl1:J1:1e6:result4:MISS4:prevd5:coordl1:J2:10e6:result4:MISS4:prevd5:coordl1:C1:6e6:result4:MISS4:prevd5:coordl1:G2:10e6:result4:MISS4:prevd5:coordl1:D1:7e6:result3:HIT4:prevd5:coordl1:F1:5e6:result4:MISS4:prevd5:coordl1:C1:7e6:result3:HIT4:prevd5:coordl1:I1:3e6:result4:MISS4:prevd5:coordl1:I1:7e6:result4:MISS4:prevd5:coordl1:H1:1e6:result4:MISS4:prevd5:coordl1:I1:5e6:result3:HIT4:prevd5:coordl1:E1:5e6:result4:MISS4:prevd5:coordl1:G1:1e6:result4:MISS4:prevd5:coordl1:A1:8e6:result4:MISS4:prevd5:coordl1:E1:9e6:result3:HIT4:prevd5:coordl1:J1:7e6:result4:MISS4:prevd5:coordl1:C1:2e6:result3:HIT4:prevd5:coordl1:C2:10e6:result4:MISS4:prevd5:coordl1:D1:1e6:result4:MISS4:prevd5:coordl1:E1:3e6:result4:MISS4:prevd5:coordl1:F1:1e6:result4:MISS4:prevd5:coordl1:B1:7e6:result4:MISS4:prevd5:coordl1:I1:8e6:result4:MISS4:prevd5:coordl1:G1:7e6:result4:MISS4:prevd5:coordl1:A1:9e6:result4:MISS4:prevd5:coordl1:G1:9e6:result4:MISS4:prevd5:coordl1:B1:4e6:result4:MISS4:prevd5:coordl1:A1:2e6:result4:MISS4:prevd5:coordl1:F1:3e6:result4:MISS4:prevd5:coordl1:F1:6e6:result3:HIT4:prevd5:coordl1:I1:4e6:result4:MISS4:prevd5:coordl1:H1:3e6:result3:HIT4:prevd5:coordl1:I1:2e6:result4:MISS4:prevd5:coordl1:G1:6e6:result4:MISS4:prevd5:coordl1:H1:2e6:result3:HIT4:prevd5:coordl1:B1:2e6:result3:HIT4:prevd5:coordl1:D1:3e6:result4:MISS4:prevd5:coordl1:F1:3e6:result4:MISS4:prevd5:coordl1:J2:10e6:result4:MISS4:prevd5:coordl1:E1:8e6:result4:MISS4:prevd5:coordl1:H1:9e6:result4:MISS4:prevd5:coordl1:F1:4e6:result4:MISS4:prevd5:coordl1:G2:10e6:result4:MISS4:prevd5:coordl1:B1:5e6:result4:MISS4:prevd5:coordl1:H1:5e6:result4:MISS4:prevd5:coordl1:C1:6e6:result4:MISS4:prevd5:coordl1:G1:5e6:result4:MISS4:prevd5:coordl1:C1:9e6:result4:MISS4:prevd5:coordl1:E1:4e6:result3:HIT4:prevd5:coordl1:D1:5eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

score1 = countScore $ fst (fromRight (NULL, "") (parseBencode "d5:coordl1:J1:2e6:result4:MISS4:prevd5:coordl1:J1:1e6:result3:HIT4:prevd5:coordl1:A1:6e6:result4:MISS4:prevd5:coordl1:I1:8e6:result4:MISS4:prevd5:coordl1:F1:6e6:result4:MISS4:prevd5:coordl1:H1:4e6:result3:HIT4:prevd5:coordl1:J1:7e6:result4:MISS4:prevd5:coordl1:J1:5e6:result4:MISS4:prevd5:coordl1:J1:5e6:result4:MISS4:prevd5:coordl1:B1:6e6:result4:MISS4:prevd5:coordl1:E1:3e6:result4:MISS4:prevd5:coordl1:E1:1e6:result4:MISS4:prevd5:coordl1:B1:7e6:result4:MISS4:prevd5:coordl1:A1:9e6:result4:MISS4:prevd5:coordl1:I2:10e6:result4:MISS4:prevd5:coordl1:B1:4e6:result4:MISS4:prevd5:coordl1:C1:3e6:result4:MISS4:prevd5:coordl1:A1:5e6:result4:MISS4:prevd5:coordl1:H1:7e6:result4:MISS4:prevd5:coordl1:J1:3e6:result3:HIT4:prevd5:coordl1:F1:2e6:result4:MISS4:prevd5:coordl1:C1:1e6:result4:MISS4:prevd5:coordl1:H1:1e6:result4:MISS4:prevd5:coordl1:G1:8e6:result3:HIT4:prevd5:coordl1:D1:5e6:result3:HIT4:prevd5:coordl1:I1:2e6:result4:MISS4:prevd5:coordl1:I1:1e6:result4:MISS4:prevd5:coordl1:F1:1e6:result4:MISS4:prevd5:coordl1:G1:3e6:result4:MISS4:prevd5:coordl1:H1:2e6:result4:MISS4:prevd5:coordl1:J1:3e6:result3:HIT4:prevd5:coordl1:D1:8e6:result4:MISS4:prevd5:coordl1:E1:8e6:result3:HIT4:prevd5:coordl1:C1:5e6:result4:MISS4:prevd5:coordl1:B1:3e6:result4:MISS4:prevd5:coordl1:C1:4e6:result4:MISS4:prevd5:coordl1:J1:4e6:result4:MISS4:prevd5:coordl1:I1:5e6:result3:HIT4:prevd5:coordl1:I1:3e6:result4:MISS4:prevd5:coordl1:A1:7e6:result4:MISS4:prevd5:coordl1:B1:9e6:result4:MISS4:prevd5:coordl1:J1:4e6:result4:MISS4:prevd5:coordl1:A1:7e6:result4:MISS4:prevd5:coordl1:E1:4e6:result4:MISS4:prevd5:coordl1:I1:9e6:result4:MISS4:prevd5:coordl1:H1:5e6:result4:MISS4:prevd5:coordl1:A1:3e6:result4:MISS4:prevd5:coordl1:I1:7e6:result4:MISS4:prevd5:coordl1:C1:9e6:result4:MISS4:prevd5:coordl1:F1:9e6:result4:MISS4:prevd5:coordl1:A1:2e6:result4:MISS4:prevd5:coordl1:C1:2e6:result4:MISS4:prevd5:coordl1:D1:4e6:result3:HIT4:prevd5:coordl1:B1:8e6:result4:MISS4:prevd5:coordl1:A1:4e6:result4:MISS4:prevd5:coordl1:F2:10e6:result4:MISS4:prevd5:coordl1:F1:9e6:result3:HIT4:prevd5:coordl1:I1:4e6:result4:MISS4:prevd5:coordl1:I1:6e6:result3:HIT4:prevd5:coordl1:H2:10e6:result3:HIT4:prevd5:coordl1:H2:10e6:result4:MISS4:prevd5:coordl1:G1:1e6:result4:MISS4:prevd5:coordl1:A1:8e6:result3:HIT4:prevd5:coordl1:F1:2e6:result3:HIT4:prevd5:coordl1:C1:5e6:result4:MISS4:prevd5:coordl1:D1:4e6:result4:MISS4:prevd5:coordl1:H1:4e6:result4:MISS4:prevd5:coordl1:J1:6e6:result3:HIT4:prevd5:coordl1:B1:2e6:result4:MISS4:prevd5:coordl1:I1:9e6:result4:MISS4:prevd5:coordl1:G1:9e6:result4:MISS4:prevd5:coordl1:C1:3e6:result3:HIT4:prevd5:coordl1:D1:8e6:result4:MISS4:prevd5:coordl1:H1:9e6:result3:HIT4:prevd5:coordl1:F1:5e6:result4:MISS4:prevd5:coordl1:E1:9e6:result4:MISS4:prevd5:coordl1:F1:4e6:result4:MISS4:prevd5:coordl1:B1:9e6:result4:MISS4:prevd5:coordl1:G1:4e6:result4:MISS4:prevd5:coordl1:A2:10e6:result4:MISS4:prevd5:coordl1:F1:7e6:result4:MISS4:prevd5:coordl1:G1:4e6:result3:HIT4:prevd5:coordl1:B1:1e6:result4:MISS4:prevd5:coordl1:F1:8e6:result4:MISS4:prevd5:coordl1:F2:10e6:result4:MISS4:prevd5:coordl1:J1:2e6:result4:MISS4:prevd5:coordl1:D1:6e6:result4:MISS4:prevd5:coordl1:D1:2e6:result4:MISS4:prevd5:coordl1:B1:5e6:result4:MISS4:prevd5:coordl1:B1:3e6:result4:MISS4:prevd5:coordl1:E1:2e6:result4:MISS4:prevd5:coordl1:B2:10e6:result4:MISS4:prevd5:coordl1:G1:7e6:result4:MISS4:prevd5:coordl1:I1:1e6:result3:HIT4:prevd5:coordl1:B1:8e6:result4:MISS4:prevd5:coordl1:E1:2e6:result4:MISS4:prevd5:coordl1:H1:6e6:result3:HIT4:prevd5:coordl1:J1:8e6:result4:MISS4:prevd5:coordl1:G1:2e6:result3:HIT4:prevd5:coordl1:C1:8e6:result4:MISS4:prevd5:coordl1:C1:1e6:result4:MISS4:prevd5:coordl1:E2:10e6:result4:MISS4:prevd5:coordl1:B1:6e6:result4:MISS4:prevd5:coordl1:D1:9e6:result4:MISS4:prevd5:coordl1:B2:10e6:result4:MISS4:prevd5:coordl1:J1:9e6:result4:MISS4:prevd5:coordl1:J1:9e6:result4:MISS4:prevd5:coordl1:H1:6e6:result3:HIT4:prevd5:coordl1:C2:10e6:result4:MISS4:prevd5:coordl1:H1:8e6:result4:MISS4:prevd5:coordl1:D1:9e6:result4:MISS4:prevd5:coordl1:G1:3e6:result4:MISS4:prevd5:coordl1:E2:10e6:result4:MISS4:prevd5:coordl1:D1:6e6:result4:MISS4:prevd5:coordl1:H1:3e6:result3:HIT4:prevd5:coordl1:D2:10e6:result4:MISS4:prevd5:coordl1:G1:6e6:result4:MISS4:prevd5:coordl1:G1:2e6:result4:MISS4:prevd5:coordl1:A1:5e6:result4:MISS4:prevd5:coordl1:D1:1e6:result4:MISS4:prevd5:coordl1:H1:8e6:result3:HIT4:prevd5:coordl1:B1:1e6:result3:HIT4:prevd5:coordl1:C1:8e6:result3:HIT4:prevd5:coordl1:A1:6e6:result4:MISS4:prevd5:coordl1:E1:6e6:result4:MISS4:prevd5:coordl1:C1:7e6:result4:MISS4:prevd5:coordl1:J1:6e6:result4:MISS4:prevd5:coordl1:F1:7e6:result3:HIT4:prevd5:coordl1:D2:10e6:result4:MISS4:prevd5:coordl1:G1:5e6:result3:HIT4:prevd5:coordl1:E1:5e6:result4:MISS4:prevd5:coordl1:I2:10e6:result4:MISS4:prevd5:coordl1:F1:8e6:result4:MISS4:prevd5:coordl1:A1:3e6:result4:MISS4:prevd5:coordl1:E1:1e6:result4:MISS4:prevd5:coordl1:E1:7e6:result4:MISS4:prevd5:coordl1:E1:7e6:result4:MISS4:prevd5:coordl1:A1:1e6:result4:MISS4:prevd5:coordl1:J1:1e6:result4:MISS4:prevd5:coordl1:J2:10e6:result4:MISS4:prevd5:coordl1:C1:6e6:result4:MISS4:prevd5:coordl1:G2:10e6:result4:MISS4:prevd5:coordl1:D1:7e6:result3:HIT4:prevd5:coordl1:F1:5e6:result4:MISS4:prevd5:coordl1:C1:7e6:result3:HIT4:prevd5:coordl1:I1:3e6:result4:MISS4:prevd5:coordl1:I1:7e6:result4:MISS4:prevd5:coordl1:H1:1e6:result4:MISS4:prevd5:coordl1:I1:5e6:result3:HIT4:prevd5:coordl1:E1:5e6:result4:MISS4:prevd5:coordl1:G1:1e6:result4:MISS4:prevd5:coordl1:A1:8e6:result4:MISS4:prevd5:coordl1:E1:9e6:result3:HIT4:prevd5:coordl1:J1:7e6:result4:MISS4:prevd5:coordl1:C1:2e6:result3:HIT4:prevd5:coordl1:C2:10e6:result4:MISS4:prevd5:coordl1:D1:1e6:result4:MISS4:prevd5:coordl1:E1:3e6:result4:MISS4:prevd5:coordl1:F1:1e6:result4:MISS4:prevd5:coordl1:B1:7e6:result4:MISS4:prevd5:coordl1:I1:8e6:result4:MISS4:prevd5:coordl1:G1:7e6:result4:MISS4:prevd5:coordl1:A1:9e6:result4:MISS4:prevd5:coordl1:G1:9e6:result4:MISS4:prevd5:coordl1:B1:4e6:result4:MISS4:prevd5:coordl1:A1:2e6:result4:MISS4:prevd5:coordl1:F1:3e6:result4:MISS4:prevd5:coordl1:F1:6e6:result3:HIT4:prevd5:coordl1:I1:4e6:result4:MISS4:prevd5:coordl1:H1:3e6:result3:HIT4:prevd5:coordl1:I1:2e6:result4:MISS4:prevd5:coordl1:G1:6e6:result4:MISS4:prevd5:coordl1:H1:2e6:result3:HIT4:prevd5:coordl1:B1:2e6:result3:HIT4:prevd5:coordl1:D1:3e6:result4:MISS4:prevd5:coordl1:F1:3e6:result4:MISS4:prevd5:coordl1:J2:10e6:result4:MISS4:prevd5:coordl1:E1:8e6:result4:MISS4:prevd5:coordl1:H1:9e6:result4:MISS4:prevd5:coordl1:F1:4e6:result4:MISS4:prevd5:coordl1:G2:10e6:result4:MISS4:prevd5:coordl1:B1:5e6:result4:MISS4:prevd5:coordl1:H1:5e6:result4:MISS4:prevd5:coordl1:C1:6e6:result4:MISS4:prevd5:coordl1:G1:5e6:result4:MISS4:prevd5:coordl1:C1:9e6:result4:MISS4:prevd5:coordl1:E1:4e6:result3:HIT4:prevd5:coordl1:D1:5eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"))
