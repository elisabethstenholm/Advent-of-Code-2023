module Utils (Parser, choice, tryDefault, many', sepBy') where

import Control.Applicative
import Data.Void (Void)
import Text.Megaparsec (Parsec, try)

type Parser = Parsec Void String

choice :: (Alternative f, Foldable t, Functor t) => t a -> f a
choice = foldl (<|>) empty . (pure <$>)

tryDefault :: Parser a  -- ^ Attempt this parser first.
           -> Parser b  -- ^ Fallback parser.
           -> (a -> Parser b) -- ^ Next step if successful.
           -> Parser b -- ^ Combined parser.
tryDefault a b f = ((Just <$> try a) <|> return Nothing) >>= maybe b f

many' :: Parser a -> Parser [a]
many' p = tryDefault p (return []) ((<$> many' p) . (:))

sepBy' :: Parser a -> Parser sep -> Parser [a]
sepBy' p sep
    = tryDefault p
                (return [])
                ((<$> (many' (sep >> p) <|> return [])) . (:))

