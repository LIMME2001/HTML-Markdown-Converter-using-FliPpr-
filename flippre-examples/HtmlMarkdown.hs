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
  | TagHtml HtmlExp
  | TagBold HtmlExp
  | TagH1 HtmlExp
  | TagH2 HtmlExp
  | TagH3 HtmlExp
  | TagH4 HtmlExp
  | TagH5 HtmlExp
  | Sequence HtmlExp HtmlExp
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
                , unTagHtml $ \e1 -> text "<html>" <+> pExp 0 e1 <+> text "</html>"
                , unTagBold $ \e1 -> text "<b>" <+> pExp 0 e1 <+> text "</b>"
                , unTagH1 $ \e1 -> text "<h1>" <+> pExp 0 e1 <+> text "</h1>"
                , unTagH2 $ \e1 -> text "<h2>" <+> pExp 0 e1 <+> text "</h2>"
                , unTagH3 $ \e1 -> text "<h3>" <+> pExp 0 e1 <+> text "</h3>"
                , unTagH4 $ \e1 -> text "<h4>" <+> pExp 0 e1 <+> text "</h4>"
                , unTagH5 $ \e1 -> text "<h5>" <+> pExp 0 e1 <+> text "</h5>"
                , unSequence $ \e1 e2 -> pExp 0 e1 <+> pExp 0 e2
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
                , unTagHtml $ \e1 -> text "" <+> pExp 0 e1
                , unTagBold $ \e1 -> text "**" <+> pExp 0 e1 <+> text "**"
                , unTagH1 $ \e1 -> pExp 0 e1 <+> text "\n==="
                , unTagH2 $ \e1 -> pExp 0 e1 <+> text "\n---"
                , unTagH3 $ \e1 -> text "###" <+> pExp 0 e1
                , unTagH4 $ \e1 -> text "####" <+> pExp 0 e1
                , unTagH5 $ \e1 -> text "#####" <+> pExp 0 e1
                , unSequence $ \e1 e2 -> pExp 0 e1 <+> text "\n" <+> pExp 0 e2
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


-- Example HTML expressions:

-- Example 1: Simple HTML structure <html>helloWorld</html>
htmlExp1 :: HtmlExp
htmlExp1 = 
  TagHtml (Content (Name "helloWorld"))

-- Example 2: HTML with bold and header
-- <html><b>helloWorld</b><h1>helloWorld</h1></html>
htmlExp2 :: HtmlExp
htmlExp2 = 
  TagHtml (Sequence 
    (TagBold (Content (Name "helloWorld")))
    (TagH1 (Content (Name "helloWorld")))
  )

-- Example 3: Bold text <b>helloWorld</b>
htmlExp3 :: HtmlExp
htmlExp3 = 
  TagBold (Content (Name "helloWorld"))

-- Example 4: Header level 3 <h3>helloWorld</h3>
htmlExp4 :: HtmlExp
htmlExp4 = 
  TagH3 (Content (Name "helloWorld"))


-- Corresponding raw HTML text for comparison (without spaces would be optimal):
-- TODO: make sure it works without spaces as well (as html is written)
htmlTextDoc :: Doc ann
htmlTextDoc = text "<html> helloWorld </html>"

htmlTextDoc2 :: Doc ann
htmlTextDoc2 = text "<html> <b> helloWorld </b> </html>"

htmlTextDoc3 :: Doc ann
htmlTextDoc3 = text "<h1> helloWorld </h1>"

main :: IO ()
main = do
  -- Function to test a single HtmlExp example
  let testExample example name = do
        putStrLn $ replicate 80 '+'
        putStrLn $ "Testing: " ++ name
        putStrLn $ replicate 80 '-'

        -- Pretty-print example as HTML
        let htmlDoc = show (pprHtmlExp1 example)
        putStrLn "HTML Output:"
        putStrLn htmlDoc

        -- Parse the HTML output back to HtmlExp
        let parsedHtmlExp = parseHtmlTextDoc1 htmlDoc
        putStrLn "Parsed HtmlExp (from HTML):"
        print parsedHtmlExp

        -- Verify that parsing round-trips correctly
        putStrLn "Round-Trip (HTML):"
        print (parsedHtmlExp == example)

        -- Convert HtmlExp to Markdown
        let markdownDoc = show (pprHtmlExp2 example)
        putStrLn "Markdown Output:"
        putStrLn markdownDoc

        -- Parse Markdown back to HtmlExp using parseHtmlTextDoc2
        let parsedMarkdownExp = parseHtmlTextDoc2 markdownDoc
        putStrLn "Parsed HtmlExp (from Markdown):"
        print parsedMarkdownExp

        -- Verify that Markdown parsing round-trips correctly
        putStrLn "Round-Trip (Markdown):"
        print (parsedMarkdownExp == example)

  -- Test each example
  testExample htmlExp1 "Example 1: <html>helloWorld</html>"
  testExample htmlExp2 "Example 2: <html><b>helloWorld</b><h1>helloWorld</h1></html>"
  testExample htmlExp3 "Example 3: <b>helloWorld</b>"
  testExample htmlExp4 "Example 4: <h3>helloWorld</h3>"





{-
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

  -- TEST 2

  -- Pretty-print htmlExp1 as a html text Doc ann
  let s = show (pprHtmlExp1 htmlExp2)
  putStrLn "`pprHtmlExp1 htmlExp2` results in ..."
  putStrLn s

  -- Parse the pretty-printed htmlExp1 back to HtmlExp
  let e = parseHtmlTextDoc1 s
  putStrLn $ replicate 80 '-'
  putStrLn "`parseHtmlTextDoc1 (pprHtmlExp1 htmlExp2)` results in ..."
  print e

  -- convert htmlTextDoc to data the to markdown text Doc ann
  putStrLn $ replicate 80 '-'
  let s2 = show (pprHtmlExp2 e)
  putStrLn "`pprHtmlExp2 e2` results in ..."
  putStrLn s2

  -- Check if htmlExp1 matches the parsed result
  putStrLn $ replicate 80 '-'
  printf "`htmlExp2 == parseHtmlTextDoc1 (pprHtmlExp1 htmlExp2)` = %s\n" (show $ e == htmlExp2)

  putStrLn $ replicate 80 '+'

  -- TEST 3

  -- Pretty-print htmlExp1 as a html text Doc ann
  let s = show (pprHtmlExp1 htmlExp3)
  putStrLn "`pprHtmlExp1 htmlExp3` results in ..."
  putStrLn s

  -- Parse the pretty-printed htmlExp1 back to HtmlExp
  let e = parseHtmlTextDoc1 s
  putStrLn $ replicate 80 '-'
  putStrLn "`parseHtmlTextDoc1 (pprHtmlExp1 htmlExp3)` results in ..."
  print e

  -- Check if htmlExp1 matches the parsed result
  putStrLn $ replicate 80 '-'
  printf "`htmlExp1 == parseHtmlTextDoc1 (pprHtmlExp1 htmlExp3)` = %s\n" (show $ e == htmlExp3)

  putStrLn $ replicate 80 '+'

  -- TEST 4

  -- Pretty-print htmlExp1 as a html text Doc ann
  let s = show (pprHtmlExp1 htmlExp4)
  putStrLn "`pprHtmlExp1 htmlExp4` results in ..."
  putStrLn s

  -- Parse the pretty-printed htmlExp1 back to HtmlExp
  let e = parseHtmlTextDoc1 s
  putStrLn $ replicate 80 '-'
  putStrLn "`parseHtmlTextDoc1 (pprHtmlExp1 htmlExp4)` results in ..."
  print e

  -- Check if htmlExp1 matches the parsed result
  putStrLn $ replicate 80 '-'
  printf "`htmlExp1 == parseHtmlTextDoc1 (pprHtmlExp1 htmlExp4)` = %s\n" (show $ e == htmlExp4)

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


  -- test 2 markdown

  putStrLn $ replicate 80 '+'
  -- convert htmlTextDoc to data the to markdown text Doc ann
  putStrLn $ replicate 80 '-'
  let e2 = parseHtmlTextDoc1 (show htmlTextDoc2)
  putStrLn "`parseHtmlTextDoc1 (show htmlTextDoc)` results in ..."
  print e2
  let s2 = show (pprHtmlExp2 e2)
  putStrLn "`pprHtmlExp2 e2` results in ..."
  putStrLn s2


  -- test 3 markdown

  putStrLn $ replicate 80 '+'
  -- convert htmlTextDoc to data the to markdown text Doc ann
  putStrLn $ replicate 80 '-'
  let e2 = parseHtmlTextDoc1 (show htmlTextDoc3)
  putStrLn "`parseHtmlTextDoc1 (show htmlTextDoc)` results in ..."
  print e2
  let s2 = show (pprHtmlExp2 e2)
  putStrLn "`pprHtmlExp2 e2` results in ..."
  putStrLn s2

-}