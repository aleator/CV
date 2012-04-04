module Utils.Stream where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans

-- This module provides Monadic streams.

-- | Stream of monadic values
data Stream m a = Terminated | Value (m (a,Stream m a))

-- | Attaching side effects
sideEffect :: (Monad m) => (a -> m ()) -> Stream m a -> Stream m a
sideEffect p Terminated = Terminated
sideEffect p (Value next) = Value renext
	where
	  renext = do
                (r,n) <- next
                p r
                return (r,sideEffect p n)

-- | Repeating stream
listToStream [] = Terminated
listToStream (l:lst) = Value (return (l,listToStream lst))
repeatS x = Value (return (x,repeatS x))
repeatSM x = sequenceS (repeatS x)
-- | Create a stream by iterating a monadic action
iterateS op n = Value cont
	where
         cont = do
		 r <- op n
		 return $ (n,iterateS op r)

-- | Pure and monadic left fold over a stream
foldS op i Terminated = return i
foldS op i (Value xs) = xs >>= \(x,xn) -> foldS op (op i x) xn

foldSM op i Terminated = return i
foldSM op i (Value xs) = xs >>= \(x,xn) -> op i x >>= \opix -> foldSM op opix xn

-- | Merge two (time)streams
time (a,_) = a
value (_,a) = a
mergeTimeStreams starta startb  a b = mergeE (starta,startb) (mergeS a b)
mergeTimeStreamsWith sa sb op a b = fmap (\(t,(a,b)) -> (t,(op a b))) $ mergeTimeStreams sa sb a b
mergeManyW starts op streams = snd $ foldl1 (\(s,m) (s1,n) -> ((op s s1),mergeTimeStreamsWith s s1 op m n)) (zip starts streams)

mergeS Terminated _ = Terminated
mergeS _ Terminated = Terminated
mergeS (Value xs) (Value ys) = Value renext
    where
        renext = do
            (x,xn) <- xs
            (y,yn) <- ys
            case compare (time x) (time y) of
                LT -> return (L x,mergeS xn (push y yn))
                EQ -> return (B (time x,(value x,value y)),mergeS xn yn)
                GT -> return (R y,mergeS (push x xn) yn)

data LRB a b c = L a | B b |  R c  deriving (Show)

mergeE _ Terminated = Terminated
mergeE (l,r) (Value xs) = Value renext
    where
        renext = do
                   (x,xn) <- xs
                   case x of
                    L (t,a) -> return ((t,(a,r)),mergeE (a,r) xn)
                    B (t,(a,b)) -> return ((t,(a,b)),mergeE (a,b) xn)
                    R (t,b) -> return ((t,(l,b)),mergeE (l,b) xn)

push x Terminated = Value (return (x,Terminated))
push x xs = Value (return (x,xs))


-- | Map over a stream
instance (Monad m) => Functor (Stream m) where
    fmap _ Terminated   = Terminated
    fmap f (Value next) = Value renext
	where
	  renext = do
		    (r,n) <- next
		    return (f r,fmap f n)

instance (Monad m) => Applicative (Stream m) where
    pure f  = repeatS f
    Terminated <*> _ = Terminated
    _ <*> Terminated = Terminated
    (Value a) <*> (Value b) = Value renext
      where
      renext = do
        (fun,anext) <- a
        (br,bnext)  <- b
        return (fun br,anext<*>bnext)

zipS a b = (,) <$> a <*> b

--
sequenceS :: (Monad m) => Stream m (m a) -> (Stream m a)
sequenceS Terminated = Terminated
sequenceS (Value next) = Value $ do
			    (op,n) <- next
			    r <- op
		            return (r,sequenceS n)

mapMS :: (Monad m) => (a -> m b) -> Stream m a -> Stream m b
mapMS op s = sequenceS . fmap op $ s

-- |Drop elements from the stream. Due to stream structure, this operation cannot
--  fail gracefully when dropping more elements than what is found in the stream
dropS :: (Monad m) => Int -> Stream m a -> Stream m a
dropS _ Terminated = Terminated
dropS n next = Value renext
	where
         drop 0 s = return s
         drop _ Terminated = return Terminated
         drop n (Value next) =  do
            (r,ne) <- next
            drop (n-1) ne
         renext = do
            r <- drop n next
            case r of
                Terminated -> error "Not enough elements to drop"
                Value x -> x

takeS :: (Monad m) => Int -> Stream m a -> Stream m a
takeS _ Terminated = Terminated
takeS n (Value next) = Value renext
	where
         renext = do
		   (r,ne) <- next
		   if n<1 then return (r,Terminated)
		   	     else return (r,takeS (n-1) ne)

takeWhileS _ Terminated = Terminated
takeWhileS c (Value next) = Value renext
	where
         renext = do
		   (r,ne) <- next
		   if not . c $ r then return (r,Terminated)
		   	      else return (r,takeWhileS c ne)

consS a Terminated = Value (return (a, Terminated))
consS a s  = Value (return (a, s))

-- pairS is safe only for infinite streams
pairS :: (Monad m) => Stream m a -> Stream m (a,a)
pairS Terminated = Terminated
pairS (Value next) = Value renext
	where
         renext = do
            (val1,nexts2) <- next
            case nexts2 of
                Terminated    -> return (undefined,Terminated)
                (Value next2) -> do (val2,next3) <- next2
                                    return ((val1,val2),pairS (consS val2 next3))


terminateOn :: (Monad m) => (a -> Bool) -> Stream m a -> Stream m a
terminateOn cond Terminated = Terminated
terminateOn cond (Value next) = Value renext
	where
         renext = do
		   (r,n) <- next
		   if cond r then return (r,Terminated)
		   	     else return (r,terminateOn cond n)

runStream Terminated  = return []
runStream (Value s) = do
			 (n,next) <- s
			 r<-runStream next
			 return (n:r)

runStream_ Terminated  = return ()
runStream_ (Value s) = do
			 (n,next) <- s
			 runStream_ next

runLast l Terminated  = return l
runLast l (Value s) = do
   	 (n,next) <- s
   	 runLast n next

runLast1 s = runLast (error "Empty Stream") s

