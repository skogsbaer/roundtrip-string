{-# LANGUAGE FlexibleContexts, TypeSynonymInstances #-}
module Text.Roundtrip.Parser (

  module Text.Parsec, Pos.newPos, Pos.initialPos,

  PParser, parsecApply, parsecConcat, parsecAlternative1Lookahead,
  parsecAlternativeInfLookahead, parsecEmpty, parsecPure,

  runStringParser, P.runParser, mkParseError

) where

import Control.Monad.Identity (Identity, runIdentity)

import Text.Parsec hiding (runParser)
import qualified Text.Parsec as P
import Text.Parsec.Char
import qualified Text.Parsec.Pos as Pos
import qualified Text.Parsec.Prim as Prim
import qualified Text.Parsec.Error as Perror
import Text.Parsec.Prim ()

import Text.Roundtrip

type PParser s u m = ParsecT s u m

parsecApply :: Iso a b -> PParser s u m a -> PParser s u m b
parsecApply iso p =
    do a <- p
       case apply iso a of
         Just b -> return b
         Nothing -> fail $ isoFailedErrorMessageL iso a

parsecConcat :: PParser s u m a -> PParser s u m b -> PParser s u m (a, b)
parsecConcat p q =
    do x <- p
       y <- q
       return (x, y)

parsecAlternative1Lookahead :: PParser s u m a -> PParser s u m a -> PParser s u m a
parsecAlternative1Lookahead p q = p P.<|> q

parsecAlternativeInfLookahead :: PParser s u m a -> PParser s u m a -> PParser s u m a
parsecAlternativeInfLookahead p q = try p P.<|> q

parsecEmpty :: PParser s u m a
parsecEmpty = parserZero

parsecPure :: a -> PParser s u m a
parsecPure x = return x

instance Monad m => IsoFunctor (PParser s u m) where
    (<$>) = parsecApply

instance Monad m => ProductFunctor (PParser s u m) where
    (<*>) = parsecConcat

instance Monad m => Alternative (PParser s u m) where
    (<|>) = parsecAlternative1Lookahead
    (<||>) = parsecAlternativeInfLookahead
    empty = parsecEmpty

instance Monad m => Syntax (PParser s u m) where
    pure = parsecPure

instance (Monad m, Stream s m Char) => StringSyntax (PParser s u m) where
    token f = Prim.tokenPrim showChar nextPos testChar
        where
          showChar x      = '\'' : x : ['\'']
          testChar x      = if f x then Just x else Nothing
          nextPos pos x _ = Pos.updatePosChar pos x

runStringParser :: Stream s Identity Char => PParser s () Identity a -> SourceName -> s -> Either ParseError a
runStringParser p src s = runIdentity $ Prim.runParserT p () src s

mkParseError :: SourcePos -> String -> ParseError
mkParseError pos msg = Perror.newErrorMessage (Perror.Message msg) pos
