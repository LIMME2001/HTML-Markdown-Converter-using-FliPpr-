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

-- The data type for representing HTML expressions
data HtmlExp
  = Content Name 
  | TagLeft Name HtmlExp HtmlExp
  | TagRight Name
  deriving (Eq, Show)

$(mkUn ''Name)
$(mkUn ''HtmlExp)
$(mkUn ''Lit)

otherwiseP :: (arg HtmlExp -> exp t) -> Branch arg exp HtmlExp t
otherwiseP = Branch (PartialBij "otherwiseP" Just Just)

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

-- Define how to convert between HtmlExp and raw text html Doc ann
flipprExp1 :: (FliPprD arg exp) => FliPprM exp (A arg HtmlExp -> E exp D)
flipprExp1 = do
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

gExp1 :: (G.GrammarD Char g) => g (Err ann HtmlExp)
gExp1 = parsingMode (flippr $ fromFunction <$> flipprExp1)

parseHtmlTextDoc1 :: [Char] -> HtmlExp
parseHtmlTextDoc1 = \s -> case p s of
  Ok r -> head r
  Fail e -> error (show e)
  where
    -- This assignment is important; otherwise, gExp1 is evaluated again for calls of parseHtmlTextDoc1.
    p = E.parse gExp1

pprHtmlExp1 :: HtmlExp -> Doc ann
pprHtmlExp1 = pprMode (flippr $ fromFunction <$> flipprExp1)
  
-- Define how to convert between HtmlExp and markdown text Doc ann
flipprExp2 :: (FliPprD arg exp) => FliPprM exp (A arg HtmlExp -> E exp D)
flipprExp2 = do
  pprName <- share $ \x -> case_ x [unName $ \s -> textAs (s) ident]

  let pprVar = pprName

  letrs [0] $ \pExp ->
    def
      ( \prec x ->
            case_
                x
                [ unContent $ \n -> pprVar n
                , unTagLeft $ \n e1 e2 -> text "**" <+> pprVar n <+> text "**" <+> pExp 0 e1 <+> pExp 0 e2
                , unTagRight $ \n -> text "**" <+> pprVar n <+> text "**"
                , otherwiseP $ parens . pExp 0
                ]
      )
      ( return (pExp 0) )

gExp2 :: (G.GrammarD Char g) => g (Err ann HtmlExp)
gExp2 = parsingMode (flippr $ fromFunction <$> flipprExp2)

pprHtmlExp2 :: HtmlExp -> Doc ann
pprHtmlExp2 = pprMode (flippr $ fromFunction <$> flipprExp2)

parseHtmlTextDoc2 :: [Char] -> HtmlExp
parseHtmlTextDoc2 = \s -> case p s of
  Ok r -> head r
  Fail e -> error (show e)
  where
    -- This assignment is important; otherwise, gExp2 is evaluated again for calls of parseHtmlTextDoc1.
    p = E.parse gExp2


-- Example HTML expression

htmlExp1 :: HtmlExp
htmlExp1 = 
  TagLeft (Name "html") (Content (Name "helloWorld")) (TagRight (Name "html"))

htmlTextDoc :: Doc ann
htmlTextDoc = text "< html > helloWorld </ html >"

main :: IO ()
main = do
  ---------- HTML -----------

  -- Pretty-print htmlExp1 as a html text Doc ann
  let s = show (pprHtmlExp1 htmlExp1)
  putStrLn "`pprHtmlExp1 htmlExp1` results in ..."
  putStrLn s

  -- Parse the pretty-printed htmlExp1 back to HtmlExp
  let e = parseHtmlTextDoc1 s
  putStrLn $ replicate 80 '-'
  putStrLn "`parseHtmlTextDoc1 (pprHtmlExp1 htmlExp1)` results in ..."
  print e

  -- Check if htmlExp1 matches the parsed result
  putStrLn $ replicate 80 '-'
  printf "`htmlExp1 == parseHtmlTextDoc1 (pprHtmlExp1 htmlExp1)` = %s\n" (show $ e == htmlExp1)

  putStrLn $ replicate 80 '+'

  ---------- MARKDOWN -----------

  -- Pretty-print htmlExp1 as a markdown text Doc ann
  let s = show (pprHtmlExp2 htmlExp1)
  putStrLn "`pprHtmlExp2 htmlExp1` results in ..."
  putStrLn s

  -- Parse the pretty-printed htmlExp1 back to HtmlExp
  let e = parseHtmlTextDoc2 s
  putStrLn $ replicate 80 '-'
  putStrLn "`parseHtmlTextDoc1 (pprHtmlExp2 htmlExp1)` results in ..."
  print e

  -- Check if htmlExp1 matches the parsed result
  putStrLn $ replicate 80 '-'
  printf "`htmlExp1 == parseHtmlTextDoc1 (pprHtmlExp1 htmlExp1)` = %s\n" (show $ e == htmlExp1)

  -- LAST PART from raw html to raw markdown

  putStrLn $ replicate 80 '+'
  -- convert htmlTextDoc to data the to markdown text Doc ann
  putStrLn $ replicate 80 '-'
  let e2 = parseHtmlTextDoc1 (show htmlTextDoc)
  putStrLn "`parseHtmlTextDoc1 (show htmlTextDoc)` results in ..."
  print e2
  let s2 = show (pprHtmlExp2 e2)
  putStrLn "`pprHtmlExp2 e2` results in ..."
  putStrLn s2
