module Repl where

import System.IO (hFlush,stdout)

foldUntilM :: Monad m => (a -> Bool) -> (a -> a -> a) -> m a -> m a
foldUntilM pred f m = m >>= go
    where
        go x = if pred x then return x
               else do
                   y <- m
                   go (f x y)

repl :: Monad m => String               -- Prompt
                -> (String -> Bool)     -- Quit?
                -> (String -> Bool)     -- Line is ready to be evaled
                -> (String -> m String) -- Interpret, generate effect
                -> (m String -> String) -- Write output
                -> m ()                 -- Initial state
                -> IO (m ())
repl prompt shouldQuit isReady eval printOut state = do
    putStr prompt
    hFlush stdout
    line <- foldUntilM isReady (\x y -> x++" "++y) getLine
    if shouldQuit line then
        return state
    else do
        let state' = state >> eval line
        let output = printOut state'
        putStrLn output
        repl prompt shouldQuit isReady eval printOut 
             (state' >> return ())

