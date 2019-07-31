{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Effect where

class Monad m => Effect r m where
  eff :: r a -> m a
