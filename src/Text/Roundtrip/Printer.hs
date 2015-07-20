{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
module Text.Roundtrip.Printer (

  Printer(..),
  printerApply, printerConcat, printerAlternative, printerEmpty,
  printerPure, runPrinter, runStringPrinter

) where

import Control.Monad.Identity (Identity, runIdentity)

import Data.Monoid

import Text.Roundtrip

newtype Printer m r a = Printer { unPrinter :: a -> m (Maybe r) }

instance Monad m => IsoFunctor (Printer m r) where
    (<$>) = printerApply

printerApply :: Monad m => Iso a b -> Printer m r a -> Printer m r b
printerApply iso (Printer p) = Printer $ \b ->
    case unapply iso b of
      Just x -> p x
      Nothing -> return Nothing

instance (Monad m, Monoid r) => ProductFunctor (Printer m r) where
    (<*>) = printerConcat

printerConcat :: (Monoid r, Monad m) => Printer m r a -> Printer m r b -> Printer m r (a, b)
printerConcat (Printer p) (Printer q) = Printer $ \(a, b) ->
    do ma <- p a
       case ma of
         Nothing -> return Nothing
         Just !ea -> do mb <- q b
                        case mb of
                          Nothing -> return Nothing
                          Just eb -> return (Just (ea `mappend` eb))

instance Monad m => Alternative (Printer m r) where
    (<|>) = printerAlternative
    (<||>) = printerAlternative
    empty = printerEmpty

printerEmpty :: Monad m => Printer m r a
printerEmpty = Printer $ \_ -> return Nothing

printerAlternative :: Monad m => Printer m r a -> Printer m r a -> Printer m r a
printerAlternative (Printer p) (Printer q) = Printer $ \a ->
    do ma <- p a
       case ma of
         Nothing -> q a
         Just ea -> return (Just ea)

instance (Monad m, Monoid r) => Syntax (Printer m r) where
    pure = printerPure

printerPure :: (Monad m, Monoid r, Eq a) => a -> Printer m r a
printerPure x = Printer $ \y -> if x == y then return (Just mempty) else return Nothing

instance Monad m => StringSyntax (Printer m String) where
  token f = Printer $ \c -> return (if f c then Just [c] else Nothing)

runPrinter :: Printer Identity r a -> a -> Maybe r
runPrinter (Printer p) x = runIdentity (p x)

runStringPrinter :: Printer Identity String a -> a -> Maybe String
runStringPrinter = runPrinter
