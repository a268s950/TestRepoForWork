import Control.Monad
import Control.Applicative

type Env = [(String, FBAEVal)]
--type Env = [(String,Int)]

instance Functor (Reader e) where
  fmap = liftM

instance Applicative (Reader e) where
  pure = return
  (<*>) = ap

instance Monad (Reader e) where
  return x = Reader $ \e -> x
  g >>= f = Reader $ \e -> runR (f (runR g e)) e

evalM :: FBAE -> Reader Env FBAEVal
evalM (Num x) = return (NumV x)
evalM (Plus l r) = do { (NumV l') <- (evalM l) ;
                        (NumV r') <- (evalM r) ;
                        return (NumV (l'+r')) }
evalM (Minus l r) = do { (NumV l') <- (evalM l) ;
                         (NumV r') <- (evalM r) ;
                         return (NumV (l'-r')) }
--evalM env (Bind i v b) = do { v' <- evalM v ;
--                              evalM ((i,v'):env) b }
evalM (Bind i v b) = do { v' <- evalM v ;
                          local (addVar i v') (evalM b) }
--evalM env (App f a) = do { (Lambda i b) <- (evalM env f) ;
--                           a' <- (evalM env a) ;
--                           evalM ((i,a'):env) b }
--evalM (App f a) = do { (ClosureV i b e) <-( evalM f);
--                       a' <- evalM a;
--		       local (useClosure i a' e) (evalM b) }
--evalM (Id id) = do { env <- ask ;
--                     return case (lookup id env) of
--  
evalM (Lambda i t b) = do { env <- ask;
                       return (ClosureV i b env) }
evalM (App f a) = do { (ClosureV i b e) <-( evalM f);
                       a' <- evalM a;
                       local (useClosure i a' e) (evalM b) }
evalM (Id id) = do { env <- ask;
                     return (lookupName id env)
                   }
--evalM env (If c t e) = do { (Num c') <- (evalM env c) ;
--                            if c'==0 then (evalM env t) else (evalM env e)}

addVar :: String -> FBAEVal -> Env -> Env
addVar s i e = (s,i):e

useClosure :: String -> FBAEVal -> Env -> Env -> Env
useClosure i v e _ = (i,v):e

data FBAE = Num Int |
            Plus FBAE FBAE |
            Minus FBAE FBAE |
            Bind String FBAE FBAE |
            Lambda String FBAETy FBAE |
            App FBAE FBAE |
            Id String
          deriving (Show, Eq)

data FBAETy = TNum |
              TBool |
              TFunc FBAETy FBAETy
            deriving (Show, Eq)

data FBAEVal = NumV Int |
               ClosureV String FBAE Env
             deriving (Show,Eq)

{-
data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> FBAETy -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  deriving (Show, Eq)

data FBAETy where
  TNum :: FBAETy
  TFun :: FBAETy -> FBAETy -> FBAETy
  deriving (Show,Eq)

data FBAEVal where
  NumV :: Int -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)
-}
data Reader e a = Reader (e -> a)

runR :: Reader e a -> e -> a
runR (Reader f) e = f e

--g >>= f :: M a -> (a -> M b) -> M b
--g >>= f = Reader $ \e -> runR (f (runR g e)) e

ask :: Reader a a
ask = Reader $ \e -> e

asks :: (e -> a) -> Reader e a
asks f = ask >>= \e -> (return (f e))

lookupName :: String -> Env -> FBAEVal
lookupName s e = case (lookup s e) of
                   Just x -> x
                   Nothing -> error "name not found"

--useClosure creates a new environment by adding the new binding --needed for evaluating App to the environment from the closure

local :: (e -> t) -> Reader t a -> Reader e a
local f r = ask >>= \e -> return (runR r (f e))
