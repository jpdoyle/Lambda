module Parse where

import Lambda
import qualified Text.Parsec as P

lambdaExpr :: P.Parsec String () Lambda
lambdaExpr = P.try app P.<|> parenExpr P.<|> lambda P.<|> name

parenExpr :: P.Parsec String () Lambda
parenExpr = do
    P.char '('
    P.spaces
    e <- lambdaExpr
    P.spaces
    P.char ')'
    return e

varName :: P.Parsec String () String
varName = fmap (:"") P.alphaNum
    -- fmap (:"") $ P.oneOf (['a'..'z']++['A'..'Z'])
    -- c1 <- P.letter
    -- s  <- P.many P.alphaNum
    -- prime <- P.option "" $ P.string "'"
    -- return $ [c1] ++ s ++ prime

name :: P.Parsec String () Lambda
name = do
    n <- varName
    return $ Name n

lambda :: P.Parsec String () Lambda
lambda = do
    P.char '\\'
    P.spaces
    param <- varName
    P.spaces
    P.char '.'
    P.spaces
    e <- lambdaExpr
    return $ Lam param e

app :: P.Parsec String () Lambda
app = do
    f <- parenExpr P.<|> name
    P.spaces
    x <- P.many1 $ do
        term <- parenExpr P.<|> lambda P.<|> name
        P.spaces
        return term
    -- use foldl to force left-associative application
    return $ foldl Ap f x

