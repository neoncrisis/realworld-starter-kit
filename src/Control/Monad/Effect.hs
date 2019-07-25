{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Effect where

class Effect r m where
  eff :: r a -> m a
