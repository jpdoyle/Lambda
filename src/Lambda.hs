module Lambda where

import qualified Data.Map.Strict as M
-- import qualified Text.Parsec as P
import Control.Applicative ((<*>))
import Data.Maybe (fromMaybe,fromJust)
import Data.List (find,findIndex)
import Control.Monad.State.Strict

data Lambda = Ap Lambda Lambda | Lam String Lambda | Name String
              deriving(Read,Show)

data BoundLambda = BAp BoundLambda BoundLambda | 
                        BLam String BoundLambda     | 
                        BBoundVar Int | BFreeVar String

bind :: Lambda -> BoundLambda
bind = go []
    where
        go :: [String] -> Lambda -> BoundLambda
        go scope (Ap f x) = BAp (go scope f) (go scope x)
        go scope (Lam x fx) = BLam x $ go (x:scope) fx
        go scope (Name x) = maybe (BFreeVar x) BBoundVar 
                            $ findIndex (==x) scope

unbind :: BoundLambda -> Lambda
unbind = go []
    where
        go :: [String] -> BoundLambda -> Lambda
        go scope (BAp f x) = Ap (go scope f) (go scope x)
        go scope (BLam x fx) = Lam x $ go (x:scope) fx
        go _     (BFreeVar x) = Name x
        go scope (BBoundVar i) = Name $ scope !! i

-- Removes trailing 's. eg. unprimed "x''" == "x"
unPrimed :: String -> String
unPrimed s = take primInd s
    where
        primInd = maybe len (\i -> len - i) idx
        idx = findIndex (/= '\'') $ reverse s
        len = length s

uniquifyNames :: Lambda -> Lambda
uniquifyNames l = unbind $ evalState (uniquify $ bind l) 
                                     (M.empty,M.empty)
    where
        uniquify :: BoundLambda -> 
                    State (M.Map String Int,M.Map String String) 
                          BoundLambda
        uniquify (BLam x fx) = do
            x' <- uniqueName x
            fx' <- uniquify fx
            return $ BLam x' fx'
        uniquify (BAp f x) = do
            f' <- uniquify f
            x' <- uniquify x
            return $ BAp f' x'
        uniquify (BFreeVar x) = do
            (_,free) <- get
            let base = unPrimed x
            let binding = maybe (uniqueName x) return
                                $ M.lookup base free
            x' <- binding
            (m,_) <- get
            put (m,M.insert base x' free)
            return $ BFreeVar x'
        uniquify bl = return bl
        uniqueName :: String -> 
                      State (M.Map String Int,M.Map String String) 
                            String
        uniqueName n = do
            (m,free) <- get
            let base = unPrimed n
            let count = 1 + (fromMaybe (-1) $ M.lookup base m)
            let n' = base ++ replicate count '\''
            put $ (M.insert base count m,free)
            return n'

newtype Bimap a b = Bimap (M.Map a b,M.Map b a)

bmEmpty :: Bimap a b
bmEmpty = Bimap (M.empty,M.empty)

bmFlip :: Bimap a b -> Bimap b a
bmFlip (Bimap (x,y)) = Bimap (y,x)

bmInsert :: (Ord a,Ord b) => a -> b -> Bimap a b -> Bimap a b
bmInsert x y (Bimap (mx,my)) = Bimap (M.insert x y mx,M.insert y x my)

bmLookupA :: Ord a => a -> Bimap a b -> Maybe b
bmLookupA k (Bimap (m,_)) = M.lookup k m

bmLookupB :: Ord b => b -> Bimap a b -> Maybe a
bmLookupB k = bmLookupA k . bmFlip

bmMemberA :: Ord a => a -> Bimap a b -> Bool
bmMemberA k (Bimap (m,_)) = M.member k m

bmMemberB :: Ord b => b -> Bimap a b -> Bool
bmMemberB k = bmMemberA k . bmFlip

simplifyNames :: Lambda -> Lambda
simplifyNames = simplify bmEmpty
    where
        simplify :: Bimap String String -> Lambda -> Lambda
        simplify m (Name n) = Name $ fromMaybe n $ bmLookupA n m
        simplify m (Lam x fx) = Lam x' $ simplify m' fx
            where
                m' = bmInsert x x' m
                x' = fromJust $ find (\n -> not (bmMemberB n m))
                              $ iterate (++"'") $ unPrimed x
        simplify m (Ap f x) = Ap f' x'
            where
                f' = simplify m f
                x' = simplify m x

-- betaReduce assumes any entries in the map have already been reduced
betaReduce :: M.Map String Lambda -> Lambda -> Lambda
betaReduce context (Name c) = fromMaybe (Name c) $ M.lookup c context
betaReduce context (Lam c l) = Lam c (betaReduce context l)
betaReduce context (Ap f y) = l'
    where 
        l' = case f' of
                (Lam x fx) -> let context' = M.insert x y' context
                                in betaReduce context' fx
                _          -> Ap f' y'
        f' = betaReduce context f
        y' = betaReduce context y

applyContext :: M.Map String Lambda -> Lambda -> Lambda
applyContext ctx (Name n) = fromMaybe (Name n)
                            $ fmap (applyContext ctx')
                            $ M.lookup n ctx
    where ctx' = M.delete n ctx
applyContext ctx (Lam x fx) = Lam x $ applyContext (M.delete x ctx) 
                                                   fx
applyContext ctx (Ap f x) = Ap (applyContext ctx f)
                               (applyContext ctx x)

reduce :: M.Map String Lambda -> Lambda -> Lambda
reduce ctx = simplifyNames . betaReduce M.empty
                           . uniquifyNames 
                           . applyContext ctx

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

