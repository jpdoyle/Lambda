module Lambda where

import qualified Data.Map.Strict as Map
-- import qualified Text.Parsec as P
import Control.Applicative ((<*>))
import Data.Maybe (fromMaybe)

data Lambda = Ap Lambda Lambda | Lam Char Lambda | Name Char
              deriving(Read,Show)

betaReduce :: Lambda -> Lambda
betaReduce = reduceWith Map.empty
    where
        reduceWith :: Map.Map Char Lambda -> Lambda -> Lambda
        reduceWith m (Name c) = fromMaybe (Name c) $ Map.lookup c m
        reduceWith m (Lam c l) = Lam c (reduceWith m l)
        reduceWith m (Ap f y) = l'
            where 
                l' = case f' of
                        (Lam x fx) -> let m' = Map.insert x y' m
                                          in reduceWith m' fx
                        _          -> Ap f' y'
                f' = reduceWith m f
                y' = reduceWith m y

apply :: Lambda -> Lambda -> Lambda
apply f x = betaReduce (Ap f x)


-- Church numerals!
zero :: Lambda
zero = Lam 'f' (Lam 'x' (Name 'x'))

one :: Lambda
one = (Ap lamSucc zero)

-- For checking betaReduce
one' :: Lambda 
one' = Lam 'f' (Lam 'x' (Ap (Name 'f') (Name 'x')))

lamSucc :: Lambda
lamSucc = Lam 'n' 
            (Lam 'f' 
                (Lam 'x' (Ap (Name 'f') 
                             (Ap (Ap (Name 'n') 
                                     (Name 'f')) 
                                 (Name 'x')))))

