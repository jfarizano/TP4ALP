module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let x e) = do n <- evalExp e
                        update x n
                        return Skip
stepComm (Seq Skip c1) = return c1
stepComm (Seq c0 c1) =  do c0' <- stepComm c0
                           return (Seq c0' c1)
stepComm (IfThenElse e c0 c1) = do b <- evalExp e
                                   if b then return c0
                                        else return c1
stepComm w@(While e c) = do b <- evalExp e
                            if b then return (Seq c w)
                                 else return Skip

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
-- Expresiones enteras
evalExp (Const n) = return n
evalExp (Var x) = lookfor x
evalExp (UMinus e) = do n <- evalExp e
                        return (-n)
evalExp (Plus e0 e1) = do n0 <- evalExp e0
                          n1 <- evalExp e1
                          return (n0 + n1)
evalExp (Minus e0 e1) = do n0 <- evalExp e0
                           n1 <- evalExp e1
                           return (n0 - n1)
evalExp (Times e0 e1) = do n0 <- evalExp e0
                           n1 <- evalExp e1
                           return (n0 * n1)
evalExp (Div e0 e1) = do n0 <- evalExp e0
                         n1 <- evalExp e1
                         return (n0 `div` n1)
evalExp (EAssgn x e) = do n <- evalExp e
                          update x n
                          return n
evalExp (ESeq e0 e1) = do evalExp e0
                          n <- evalExp e1
                          return n
-- Expresiones booleanas
evalExp BTrue = return True 
evalExp BFalse = return False 
evalExp (Not e) = do b <- evalExp e 
                     return (not b)
evalExp (Lt e0 e1) = do n0 <- evalExp e0
                        n1 <- evalExp e1
                        return (n0 < n1)
evalExp (Gt e0 e1) = do n0 <- evalExp e0
                        n1 <- evalExp e1
                        return (n0 > n1)
evalExp (Eq e0 e1) = do n0 <- evalExp e0
                        n1 <- evalExp e1
                        return (n0 == n1)
evalExp (NEq e0 e1) = do n0 <- evalExp e0
                         n1 <- evalExp e1
                         return (n0 /= n1)
evalExp (And e0 e1) = do b0 <- evalExp e0
                         b1 <- evalExp e1
                         return (b0 && b1)                                                     
evalExp (Or e0 e1) = do b0 <- evalExp e0
                        b1 <- evalExp e1
                        return (b0 || b1)


