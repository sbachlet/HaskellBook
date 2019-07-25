{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-} 
module STy where

data Wrap a = Wrap a
  deriving Show

data STy ty where
  SInt :: STy Int
  SBool :: STy Bool
  SMaybe :: STy ty' -> STy (Maybe ty')



zero :: STy ty -> ty
zero SInt = 0
zero SBool = False
zero (SMaybe _) = Nothing

eqSTy :: STy ty -> STy ty -> Bool
eqSTy SInt SInt = True
eqSTy SBool SBool = True
eqSTy (SMaybe a) (SMaybe b) = a `eqSTy` b
