module Lambda where

import qualified Data.Map.Strict as Map
-- import qualified Text.Parsec as P
import Control.Applicative ((<*>))
import Data.Maybe (fromMaybe)

data Lambda = Ap Lambda Lambda | Lam String Lambda | Name String
              deriving(Read,Show)

betaReduce :: Map.Map String Lambda -> Lambda -> Lambda
betaReduce context (Name c) = fromMaybe (Name c) $ 
                                Map.lookup c context
betaReduce context (Lam c l) = Lam c (betaReduce context l)
betaReduce context (Ap f y) = l'
    where 
        l' = case f' of
                (Lam x fx) -> let context' = Map.insert x y' context
                                in betaReduce context' fx
                _          -> Ap f' y'
        f' = betaReduce context f
        y' = betaReduce context y

apply :: Lambda -> Lambda -> Lambda
apply f x = betaReduce Map.empty (Ap f x)

lambdaShow :: Lambda -> String
lambdaShow (Name n) = n
lambdaShow (Lam x fx) = "\\" ++ x ++ "." ++ lambdaShow fx
lambdaShow (Ap f x) = wrapF ++ wrapX
    where
        wrapF = case f of
                    Lam _ _ -> "(" ++ showF ++ ")"
                    _       -> showF
        wrapX = case x of
                    Name _ -> showX
                    _      -> "(" ++ showX ++ ")"
        showF = lambdaShow f
        showX = lambdaShow x

-- Church numerals!
zero :: Lambda
zero = Lam "f" (Lam "x" (Name "x"))

one :: Lambda
one = (Ap lamSucc zero)

-- For checking betaReduce
one' :: Lambda 
one' = Lam "f" (Lam "x" (Ap (Name "f") (Name "x")))

lamSucc :: Lambda
lamSucc = Lam "n" 
            (Lam "f" 
                (Lam "x" (Ap (Name "f") 
                             (Ap (Ap (Name "n") 
                                     (Name "f")) 
                                 (Name "x")))))

