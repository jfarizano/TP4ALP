module Eval3
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

-- Ejercicio 3.a: Proponer una nueva monada que lleve el costo de las 
-- operaciones efectuadas en la computacion, ademas de manejar errores y 
-- estado, y de su instancia de mónada. Llamela StateErrorCost.
newtype StateErrorCost a =
  StateErrorCost { runStateErrorCost :: Env -> Either Error (Pair a (Env, Cost))}

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorCost where
  fmap = liftM

instance Applicative StateErrorCost where
  pure  = return
  (<*>) = ap

instance Monad StateErrorCost where
  return x = StateErrorCost (\s -> Right (x :!: (s, 0)))
  m >>= f = StateErrorCost (\s -> case runStateErrorCost m s of
                                    Left error -> Left error
                                    Right (v :!: (s', w)) -> case runStateErrorCost (f v) s' of
                                                                Left error -> Left error
                                                                Right (v' :!: (s'', w')) -> Right (v' :!: (s'', w + w')))

-- Ejercicio 3.c: Dar una instancia de MonadCost para StateErrorCost.
instance MonadCost StateErrorCost where
  tick = StateErrorCost (\s -> Right (() :!: (s, 1)))
  doubleTick = StateErrorCost (\s -> Right (() :!: (s, 2)))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorCost.
instance MonadError StateErrorCost where
  throw e = StateErrorCost (\s -> Left e)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorCost.
instance MonadState StateErrorCost where
  lookfor v = StateErrorCost (\s -> case M.lookup v s of 
                                      Nothing -> Left UndefVar
                                      Just x -> Right (x :!: (s, 0)))
  update v i = StateErrorCost (\s -> Right (() :!: (update' v i s, 0))) where update' = M.insert 

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorCost.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error (Env, Cost)
eval p = case runStateErrorCost (stepCommStar p) initEnv of
          Left error -> Left error
          Right (_ :!: (env, w)) -> Right (env, w)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadCost m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadCost m) => Comm -> m Comm
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
evalExp :: (MonadState m, MonadError m, MonadCost m) => Exp a -> m a
-- Expresiones enteras
evalExp (Const n) = return n
evalExp (Var x) = lookfor x
evalExp (UMinus e) = do n <- evalExp e
                        tick
                        return (-n)
evalExp (Plus e0 e1) = do n0 <- evalExp e0
                          n1 <- evalExp e1
                          tick
                          return (n0 + n1)
evalExp (Minus e0 e1) = do n0 <- evalExp e0
                           n1 <- evalExp e1
                           tick
                           return (n0 - n1)
evalExp (Times e0 e1) = do n0 <- evalExp e0
                           n1 <- evalExp e1
                           doubleTick
                           return (n0 * n1)
evalExp (Div e0 e1) = do n0 <- evalExp e0
                         n1 <- evalExp e1
                         doubleTick
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
                     tick
                     return (not b)
evalExp (Lt e0 e1) = do n0 <- evalExp e0
                        n1 <- evalExp e1
                        tick
                        return (n0 < n1)
evalExp (Gt e0 e1) = do n0 <- evalExp e0
                        n1 <- evalExp e1
                        tick
                        return (n0 > n1)
evalExp (Eq e0 e1) = do n0 <- evalExp e0
                        n1 <- evalExp e1
                        tick
                        return (n0 == n1)
evalExp (NEq e0 e1) = do n0 <- evalExp e0
                         n1 <- evalExp e1
                         tick
                         return (n0 /= n1)
evalExp (And e0 e1) = do b0 <- evalExp e0
                         b1 <- evalExp e1
                         tick
                         return (b0 && b1)                                                     
evalExp (Or e0 e1) = do b0 <- evalExp e0
                        b1 <- evalExp e1
                        tick
                        return (b0 || b1)