{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
-- To suppress warnings caused by TH code.
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.FliPpr
import qualified Text.FliPpr.Automaton as AM

import qualified Text.FliPpr.Grammar as G
import qualified Text.FliPpr.Grammar.Driver.Earley as E

import Data.String (fromString)
import Data.Char (isAlphaNum)


import Prettyprinter (Doc)
import Text.Printf

newtype Name = Name String
  deriving (Eq, Show)

data Lit
  = LString String
  deriving (Eq, Show)

data BinOp = Add | Mul
  deriving (Eq, Show)
data Exp
  = Content Name 
  | TagLeft Name Exp Exp
  | TagRight Name
  deriving (Eq, Show)

$(mkUn ''Name)
$(mkUn ''Exp)
$(mkUn ''Lit)

otherwiseP :: (arg Exp -> exp t) -> Branch arg exp Exp t
otherwiseP = Branch (PartialBij "otherwiseP" Just Just)

atoiP :: (arg String -> exp t) -> Branch arg exp Int t
atoiP =
  Branch
    ( PartialBij
        "atoi"
        (Just . show)
        (\s -> l2m $ do (n, "") <- reads s; return n)
    )
  where
    l2m [] = Nothing
    l2m (x : _) = Just x

number :: AM.DFA Char
number = AM.range '0' '9'

numbers :: AM.DFA Char
numbers = AM.plus number

ident :: AM.DFA Char
ident = (small <> AM.star alphaNum) `AM.difference` AM.unions (map fromString keywords)
  where
    small = AM.unions [AM.range 'a' 'z', AM.singleton '_']
    alphaNum = AM.unions [number, small, AM.range 'A' 'Z']


keywords :: [String]
keywords = []

flipprExp :: (FliPprD arg exp) => FliPprM exp (A arg Exp -> E exp D)
flipprExp = do
  pprName <- share $ \x -> case_ x [unName $ \s -> textAs (s) ident]

  let pprVar = pprName

  letrs [0] $ \pExp ->
    def
      ( \prec x ->
            case_
                x
                [ unContent $ \n -> pprVar n
                , unTagLeft $ \n e1 e2 -> text "<" <+> pprVar n <+> text ">" <+> pExp 0 e1 <+> pExp 0 e2
                , unTagRight $ \n -> text "</" <+> pprVar n <+> text ">"
                , otherwiseP $ parens . pExp 0
                ]
      )
      ( return (pExp 0) )

gExp :: (G.GrammarD Char g) => g (Err ann Exp)
gExp = parsingMode (flippr $ fromFunction <$> flipprExp)

parseExp :: [Char] -> Exp
parseExp = \s -> case p s of
  Ok r -> head r
  Fail e -> error (show e)
  where
    -- This assignment is important; otherwise, gExp is evaluated again for calls of parseExp.
    p = E.parse gExp

pprExp :: Exp -> Doc ann
pprExp = pprMode (flippr $ fromFunction <$> flipprExp)

exp1 :: Exp
exp1 = 
  TagLeft (Name "html") (Content (Name "helloWorld")) (TagRight (Name "html"))

exp2 :: Doc ann
exp2 = text "< html > helloWorld </ html >"

main :: IO ()
main = do
  -- Pretty-print exp1 as a Doc ann
  let s = show (pprExp exp1)
  putStrLn "`pprExp exp1` results in ..."
  putStrLn s

  -- Parse the pretty-printed exp1 back to Exp
  let e = parseExp s
  putStrLn $ replicate 80 '-'
  putStrLn "`parseExp (pprExp exp1)` results in ..."
  print e

  -- Check if exp1 matches the parsed result
  putStrLn $ replicate 80 '-'
  printf "`exp1 == parseExp (pprExp exp1)` = %s\n" (show $ e == exp1)

  -- convert exp2 to data
  putStrLn $ replicate 80 '-'
  let e2 = parseExp (show exp2)
  putStrLn "`parseExp (show exp2)` results in ..."
  print e2
