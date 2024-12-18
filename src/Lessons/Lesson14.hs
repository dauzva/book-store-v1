{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
module Lessons.Lesson14 () where

import Control.Monad.Trans.State.Strict ();
import Control.Monad.Trans.Class;
import Control.Monad.Trans.State
import Control.Monad.Trans.Except ()

newtype EitherT e m a = EitherT {
	runEitherT :: m (Either e a)
}


instance MonadTrans (EitherT e) where
	lift :: Monad m => m a -> EitherT e m a
	lift ma = EitherT $ fmap Right ma

instance Monad m => Functor (EitherT e m) where
	fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
	fmap f ta = EitherT $ do
		a <- runEitherT ta
		case a of
			Left e -> return $ Left e
			Right ar -> return $ Right (f ar)


instance Monad m => Applicative (EitherT e m) where
	pure :: a -> EitherT e m a
	pure a = EitherT $ return $ Right a
	(<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
	af <*> aa = EitherT $ do
		f <- runEitherT af
		case f of
			Left e1 -> return $ Left e1
			Right r1 -> do
				a <- runEitherT aa
				case a of
					Left e2 -> return $ Left e2
					Right r2 -> return $ Right (r1 r2)


instance Monad m => Monad (EitherT e m) where
	(>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
	m >>= k = EitherT $ do
		a <- runEitherT m
		case a of
			Left e1 -> return $ Left e1
			Right r1 ->  runEitherT(k r1)


type Parser a = EitherT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runEitherT parser)

throwE :: Monad m => e -> EitherT e m a
throwE e = EitherT $ return $ Left e

parseChar :: Char -> Parser Char
parseChar a = do
    input <- lift get
    case input of
        [] -> throwE "Empty input"
        (x:xs) -> if x == a
            then lift $ put xs >> return x
            else
                throwE $ a:" is not found"


-- >>> parse (parseTwoSameChar 'a') ""
-- (Left "Empty input","")

-- >>> parse (parseTwoSameChar 'a') "a"
-- (Left "Empty input","")

-- >>> parse (parseTwoSameChar 'a') "awb"
-- (Left "a is not found","wb")
parseTwoSameChar :: Char -> Parser(Char, Char)
parseTwoSameChar c = do
	c1 <- parseChar c
	c2 <- parseChar c
	return(c1, c2)


-- >>> [1,2,error "a",4 ] `deepseq` 5
-- Variable not in scope:
--   deepseq :: [a0_a2m0w[tau:1]] -> t0_a2m0r[tau:1] -> t_a2m0t[sk:1]
