import Control.Monad
import Control.Applicative

--from document "the typing environment....
--can be viewed as a list of (identifier,type) pairs"
type Env = [(Id, E)]
{-
data Reader e a = Reader (e -> a)

runR :: Reader e a -> e -> a
runR (Reader f) e = f e

ask :: Reader a a
ask = Reader $ \e -> e

asks :: (e -> a) -> Reader e a
asks f = ask >>= \e -> (return (f e))

lookupName :: String -> Env -> APDT
lookupName s e = case (lookup s e) of
                   Just x -> x
                   Nothing -> error "name not found"

local :: (e -> t) -> Reader t a -> Reader e a
local f r = ask >>= \e -> return (runR r (f e))

--useClosure creates a new environment by adding the new binding
--needed for evaluating App to the environment from the closure
useClosure :: String -> APDT -> Env -> Env -> Env
useClosure i v e _ = (i,v):e

instance Functor (Reader e) where
  fmap = liftM

instance Applicative (Reader e) where
  pure = return
    (<*>) = ap

instance Monad (Reader e) where
  return x = Reader $ \e -> x
    g >>= f = Reader $ \e -> runR (f (runR g e)) e
-}
--type Id = String
data Id = IdConstructor Int
    deriving(Show,Eq)

--type Place = Int
data P = Place Int
    deriving(Show,Eq)
--USM and KIM are parameterized with Strings
--not much different
--usm talks to the same attestation manager,
--kim talks to another place

data APDT = Val E
          | Var Id
          | LN APDT APDT --Sequential
          | BR APDT APDT --Parallel
          | At P APDT  
          | App APDT APDT
          | Lambda E APDT
          | SIG     --Signature term (can be seen as a function)
          | KIM P   
          | USM
    deriving(Show,Eq)
data E = SS E E
       | PP E E
       | Epsilon  --empty
       | Sig E P  --created from SIG turning
                  --prior evidence into Sig 
       | Kim P P 
       | Usm P   
       | N P
    deriving(Show,Eq) 
data BigE = LNE BigE BigE
          | BRE BigE BigE
          | SigE BigE P
          | KimE P
          | UsmE P
          | NE P
    deriving(Show,Eq)
data T = Idk BigE
       | Function T T
-- Principle Rules --

eval :: Env -> APDT -> P -> E
eval env t p = case t of
                 KIM q    -> Kim q p
                 USM      -> Usm p
                 LN t0 t1 -> let t0' = eval env t0 p in
                             let t1' = eval env t1 p in
                             (SS t0' t1')
--                 BR t0 t1 -> let t0' = eval env t0 p in
--                             let t1' = eval env t1 p in
--                             PP t0' t1
                 Val e    -> Epsilon -- i can't remember what this is supposed to be
                 Var i    -> case (lookup i env) of --variable evaluation
                             Just v  -> v
                             Nothing -> error "bad"         
--		 LN t0 t1 -> case (isValid t0) of
--                             True -> case (isValid t1) of
--                                       True  -> return (LN t0' t1, p)
--                                       False -> return ()
--               	             False -> return ()
--               BR t0 t1 -> 

                 
                 
--addVar :: String -> APDT -> Env -> Env
--addVar s i e = (s,i):e

testVal = Kim (Place 1) (Place 2)
myEnv = [(IdConstructor 1,testVal)]