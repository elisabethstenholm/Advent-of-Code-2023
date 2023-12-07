module Utils where

import Control.Applicative

choice :: (Alternative f, Foldable t, Functor t) => t a -> f a
choice = foldl (<|>) empty . (pure <$>)