{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Interpreter (runProgram, runProgram') where

import Parser.Stmt (Stmt(..), Expr(..))
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Colors (resetColor, textColor)

data Value = IntVal Int | StrVal String
    deriving (Show, Eq)


displayValue :: Value -> String
displayValue (IntVal i) = show i
displayValue (StrVal s) = s


type InterpreterState = Map String Value


interpret :: [Stmt] -> StateT InterpreterState IO ()
interpret = mapM_ interpretStmt

interpretStmt :: Stmt -> StateT InterpreterState IO ()
interpretStmt (Rem ln var) = do
    -- liftIO $ putStrLn $ "REM> " ++ var
    return ()

interpretStmt (Let ln var expr) = do
    value <- evaluateExpr expr
    modify (Map.insert var value)

interpretStmt (Print ln expr addNewLine) = do
    value <- evaluateExpr expr
    liftIO $ putStr $ displayValue value
    when addNewLine $ do
        liftIO $ putStrLn ""

        liftIO $ resetColor
        liftIO $ putStr "BASIC> "
        currentColor <- gets (Map.lookup "currentColor")
        case currentColor of
            Just (StrVal color) -> liftIO $ textColor color
            _ -> return ()    


interpretStmt (For ln loopVar startExpr endExpr loopStmts) = do
    startVal <- evaluateExpr startExpr
    endVal <- evaluateExpr endExpr
    case (startVal, endVal) of
        (IntVal s, IntVal e) -> forLoop loopVar s e loopStmts
        _ -> error "Type mismatch: For loop expects integer values"
  where
    forLoop var currentVal endVal stmts
        | currentVal > endVal = return ()  -- Exit condition
        | otherwise = do
            modify (Map.insert var (IntVal currentVal))  -- Update loop variable
            mapM_ interpretStmt stmts                    -- Execute loop body
            forLoop var (currentVal + 1) endVal stmts    -- Recursive call for next iteration

interpretStmt (If ln condExpr stmts elseStmts) = do
    condVal <- evaluateExpr condExpr
    case condVal of
        IntVal n ->
            if n /= 0   -- Non-zero integers are true
            then mapM_ interpretStmt stmts  
            else maybe (return ()) (mapM_ interpretStmt) elseStmts 

        StrVal _ -> error "Type mismatch: If condition expects an integer value"
        -- Or, if you prefer, treat any non-empty string as true and empty string as false
        -- StrVal s -> when (not $ null s) $ mapM_ interpretStmt stmts

interpretStmt (Color _ color) = do
    -- liftIO $ putStrLn $ "COLOR> " ++ color
    modify (Map.insert "currentColor" (StrVal color))
    liftIO $ textColor color


-- interpretStmt s = do
--     _ <- error $ "Not implemented " ++ show s
--     return ()


{-
Expression evaluation
-}

evaluateExpr :: Expr -> StateT InterpreterState IO Value
evaluateExpr (Var x) = gets (Map.! x)
evaluateExpr (ConstInt n) = return $ IntVal n
evaluateExpr (ConstStr str) = return $ StrVal str

evaluateExpr (Sub e1 e2) = do
    v1 <- evaluateExpr e1
    v2 <- evaluateExpr e2
    case (v1, v2) of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 - i2)  -- Adding integers
        (StrVal _, StrVal _) -> return $ IntVal 0 -- Quite BASIC return NaN
        _ -> error "Type mismatch: Cannot add integer and string"  -- Type mismatch error

evaluateExpr (Add e1 e2) = do
    v1 <- evaluateExpr e1
    v2 <- evaluateExpr e2
    case (v1, v2) of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)  -- Adding integers
        (StrVal s1, StrVal s2) -> return $ StrVal (s1 ++ s2)  -- Concatenating strings
        _ -> error "Type mismatch: Cannot add integer and string"  -- Type mismatch error

evaluateExpr (Mult e1 e2) = do
    v1 <- evaluateExpr e1
    v2 <- evaluateExpr e2
    case (v1, v2) of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 * i2)  -- Adding integers
        _ -> error "Type mismatch: Cannot multiply"  -- Type mismatch error

evaluateExpr (Div e1 e2) = do
    v1 <- evaluateExpr e1
    v2 <- evaluateExpr e2
    case (v1, v2) of
        (IntVal i1, IntVal i2) -> return $ IntVal (div i1 i2)  -- Adding integers
        _ -> error "Type mismatch: Cannot divide"  -- Type mismatch error




-- evaluateExpr (Sub e1 e2) = do
--     v1 <- evaluateExpr e1
--     v2 <- evaluateExpr e2
--     case (v1, v2) of
--         (IntVal i1, IntVal i2) -> return $ IntVal (i1 - i2)  -- Sub integers
--         (StrVal s1, StrVal s2) -> return $ StrVal (s1 ++ s2)  -- Concatenating strings
--         _ -> error "Type mismatch: Cannot add integer and string"  -- Type mismatch error

evaluateExpr (Eq e1 e2) = do
    v1 <- evaluateExpr e1
    v2 <- evaluateExpr e2
    --return (if v1 == v2 then 1 else 0)
    case (v1, v2) of
        (IntVal i1, IntVal i2) -> return $ IntVal (if i1 == i2 then 1 else 0)
        (StrVal s1, StrVal s2) -> return $ IntVal (if s1 == s2 then 1 else 0)
        _ -> return $ IntVal 0


evaluateExpr (Negate e1) = do
    v1 <- evaluateExpr e1
    --return (if v1 == v2 then 1 else 0)
    case v1 of
        (IntVal i1) -> return $ IntVal (-i1)
        _ -> error "Cannot negate string"


evaluateExpr (And e1 e2) = do
    v1 <- evaluateExpr e1
    v2 <- evaluateExpr e2
    return $ v1 `andOp` v2

andOp :: Value -> Value -> Value
andOp (IntVal a) (IntVal b) = IntVal $ if a /= 0 && b /= 0 then 1 else 0
andOp _ _ = error "Type mismatch: AND operator expects integer values"


runProgram :: [Stmt] -> IO InterpreterState
runProgram stmts = do
    resetColor
    putStrLn "\nRunning"
    result <- execStateT (interpret stmts) initialState
    resetColor
    putStrLn ""
    return result
  where
    initialState = Map.empty -- or any default initial state you prefer


-- run without color reset
runProgram' :: [Stmt] -> IO InterpreterState
runProgram' stmts = execStateT (interpret stmts) initialState
  where
    initialState = Map.empty -- or any default initial state you prefer



