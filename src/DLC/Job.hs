module DLC.Job where

data Job a = Ok a | ErrorLog String deriving Show

-- so we could only save one error log...
instance Monad Job where
    -- (>>=) :: Job a -> (a -> Job b) -> Job b
    -- here for most cases, a == b
    (ErrorLog s) >>= _ = ErrorLog s
    (Ok a) >>= f = f a

    -- return :: a -> Job a
    return = Ok
