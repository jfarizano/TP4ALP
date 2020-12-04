module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> case runStateError m s of
                                Left error -> Left error
                                Right (v :!: s') -> runStateError (f v) s')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw e = StateError (\s -> Left e)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> case M.lookup v s of 
                                  Nothing -> Left UndefVar
                                  Just x -> Right (x :!: s))
  update v i = StateError (\s -> Right (() :!: update' v i s)) where update' = M.insert 

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = case runStateError (stepCommStar p) initEnv of
          Left error -> Left error
          Right (_ :!: env) -> Right env

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
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
                         if n1 == 0 then throw DivByZero else return (n0 `div` n1)
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

