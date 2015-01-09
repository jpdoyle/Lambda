module Main where

import Lambda
import Repl (repl)
import Parse (lambdaExpr,varName)
import qualified Text.Parsec as P
import qualified Data.Map as M
import Control.Monad.State

data Statement = Assign String Lambda |
                 Expr Lambda

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
        expr = fmap Expr lambdaExpr

closedParens :: String -> Bool
closedParens = go 0
    where
        go 0 []      = True
        go n []      = False
        go n ('(':s) = go (n+1) s
        go n (')':s) = go (n-1) s
        go n (_:s)   = go n s

eval :: Statement -> State (M.Map String Lambda) String
eval (Assign var e) = do
    context <- get
    let result = reduce context e
    put (M.insert var result context)
    return $ lambdaShow result
eval (Expr l) = do
    context <- get
    return $ lambdaShow $ reduce context l

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

