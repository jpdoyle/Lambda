module Main where

import Lambda
import Repl (repl)
import Parse (lambdaExpr,varName)
import qualified Text.Parsec as P
import qualified Data.Map as M
import Control.Monad.State

data Statement = Assign String Expression |
                 Expr Expression
data Expression = Eager Lambda | Lazy Lambda

parseStmt :: P.Parsec String () Statement
parseStmt = P.try assign P.<|> expr
    where
        assign = do 
            s <- varName
            P.spaces
            P.char '='
            P.spaces
            (Expr e) <- expr
            return $ Assign s e
        expr = fmap Expr parseExpr

parseExpr :: P.Parsec String () Expression
parseExpr = eager P.<|> lazy
    where
        eager = do
            P.char '!'
            P.spaces
            l <- lambdaExpr
            return (Eager l)
        lazy = fmap Lazy lambdaExpr

closedParens :: String -> Bool
closedParens = go 0
    where
        go 0 []      = True
        go n []      = False
        go n ('(':s) = go (n+1) s
        go n (')':s) = go (n-1) s
        go n (_:s)   = go n s

evalExpr :: M.Map String Lambda -> Expression -> Lambda
evalExpr context (Eager l) = betaReduce context $
                             betaReduce context l
evalExpr _       (Lazy  l) = l

eval :: Statement -> State (M.Map String Lambda) String
eval (Assign var e) = do
    context <- get
    let result = evalExpr context e
    put (M.insert var result context)
    return $ lambdaShow result
eval (Expr e) = do
    context <- get
    let l = case e of 
                (Eager x) -> x
                (Lazy  x) -> x
    return $ lambdaShow $ betaReduce context $ betaReduce context l

printResult :: State (M.Map String Lambda) String -> String
printResult s = evalState s M.empty

emptyContext :: State (M.Map String Lambda) ()
emptyContext = put M.empty

handleError :: (Monad m,Show e) => Either e (m String) -> m String
handleError (Right s) = s
handleError (Left  e) = return $ "ERROR: " ++ show e

main :: IO ()
main = void $ repl "\\> " (=="quit") closedParens 
                   (handleError . fmap eval . P.parse parseStmt "")
                   printResult emptyContext

