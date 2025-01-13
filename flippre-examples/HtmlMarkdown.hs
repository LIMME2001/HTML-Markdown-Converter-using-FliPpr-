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

-- Import necessary libraries and modules
import Text.FliPpr
import qualified Text.FliPpr.Automaton as Automaton
import qualified Text.FliPpr.Grammar as Grammar
import qualified Text.FliPpr.Grammar.Driver.Earley as EarleyParser

import Data.String (fromString)
import Data.Char (isAlphaNum)

import Prettyprinter (Doc)
import Text.Printf

-- Define a new type for representing names
newtype Name = Name String
  deriving (Eq, Show)

-- Define a type for literals (currently only strings)
data Lit = LString String
  deriving (Eq, Show)

-- Define a type for binary operations (addition and multiplication)
data BinOp = Add | Mul
  deriving (Eq, Show)

-- The main data type for representing HTML-like expressions
data HtmlExp
  = Content Name                -- Represents plain content (text)
  | TagHtml HtmlExp             -- Represents an <html> tag
  | TagBold HtmlExp             -- Represents a <b> tag
  | TagH1 HtmlExp               -- Represents a <h1> tag
  | TagH2 HtmlExp               -- Represents a <h2> tag
  | TagH3 HtmlExp               -- Represents a <h3> tag
  | TagH4 HtmlExp               -- Represents a <h4> tag
  | TagH5 HtmlExp               -- Represents a <h5> tag
  | TagP HtmlExp                -- Represents a <p> tag
  | TagDiv HtmlExp              -- Represents a <div> tag
  -- | TagUl [HtmlExp]             -- Represents an unordered list
  | TagLi HtmlExp               -- Represents a list item
  | Sequence HtmlExp HtmlExp    -- Represents a sequence of HTML elements
  deriving (Eq, Show)

-- Generate partial bijections for the HtmlExp type
$(mkUn ''Name)
$(mkUn ''HtmlExp)
$(mkUn ''Lit)

-- A helper function for "otherwise" branches in pattern matching
otherwiseBranch :: (arg HtmlExp -> exp t) -> Branch arg exp HtmlExp t
otherwiseBranch = Branch (PartialBij "otherwiseBranch" Just Just)

-- Define finite state automata for recognizing numbers
digit :: Automaton.DFA Char
digit = Automaton.range '0' '9'

digits :: Automaton.DFA Char
digits = Automaton.plus digit

{-
-- Define a DFA for recognizing identifiers
identifier :: Automaton.DFA Char
identifier = (lowercase <> Automaton.star alphanum) `Automaton.difference` Automaton.unions (map fromString reservedKeywords)
  where
    lowercase = Automaton.unions [Automaton.range 'a' 'z', Automaton.singleton '_']
    alphanum = Automaton.unions [digit, lowercase, Automaton.range 'A' 'Z']
-}

identifier :: Automaton.DFA Char
identifier = (Automaton.star allowedChars) `Automaton.difference` Automaton.unions (map fromString reservedKeywords)
  where
    allowedChars = Automaton.unions 
      [ Automaton.range 'a' 'z'
      , Automaton.range 'A' 'Z'
      , Automaton.range '0' '9'
      , Automaton.singleton '_'
      , Automaton.singleton ' '   -- TODO: if i have this most things work but case 1 ceases to work...
      , Automaton.singleton '.'
      ]
    

-- List of reserved keywords (empty in this example)
reservedKeywords :: [String]
reservedKeywords = []

-- Define a pretty-printer for HtmlExp that generates HTML text
htmlPrettyPrinter :: (FliPprD arg exp) => FliPprM exp (A arg HtmlExp -> E exp D)
htmlPrettyPrinter = do
  -- Pretty-printer for names
  prettyName <- share $ \name -> case_ name [unName $ \str -> textAs str identifier]

  -- Use prettyName for variable-like structures
  let prettyVar = prettyName

  -- Define the pretty-printer logic for HtmlExp
  letrs [0] $ \prettyExp -> do
    def
      ( \_prec expr -> 
          case_ expr
            [ unContent $ \name -> prettyVar name
            , unTagHtml $ \child -> text "<html>" <+> prettyExp 0 child <+> text "</html>"
            , unTagBold $ \child -> text "<b>" <+> prettyExp 0 child <+> text "</b>"
            , unTagH1 $ \child -> text "<h1>" <+> prettyExp 0 child <+> text "</h1>"
            , unTagH2 $ \child -> text "<h2>" <+> prettyExp 0 child <+> text "</h2>"
            , unTagH3 $ \child -> text "<h3>" <+> prettyExp 0 child <+> text "</h3>"
            , unTagH4 $ \child -> text "<h4>" <+> prettyExp 0 child <+> text "</h4>"
            , unTagH5 $ \child -> text "<h5>" <+> prettyExp 0 child <+> text "</h5>"
            , unTagP $ \child -> text "<p>" <+> prettyExp 0 child <+> text "</p>"
            , unTagDiv $ \child -> text "<div>" <+> prettyExp 0 child <+> text "</div>"
            --, unTagUl $ \children -> text "<ul>" <+> mconcat (map (\item -> prettyExp 0 item <+> text "\n") children) <+> text "</ul>"
            , unTagLi $ \child -> text "<li>" <+> prettyExp 0 child <+> text "</li>"
            , unSequence $ \first second -> prettyExp 0 first <+> text "\n" <+> prettyExp 0 second
            , otherwiseBranch $ parens . prettyExp 0
            ]
      )
      (return (prettyExp 0))

-- Define a grammar for parsing HTML expressions
htmlGrammar :: (Grammar.GrammarD Char g) => g (Err ann HtmlExp)
htmlGrammar = parsingMode (flippr $ fromFunction <$> htmlPrettyPrinter)

-- Function to parse an HTML string into an HtmlExp
parseHtml :: String -> HtmlExp
parseHtml input = case parser input of
  Ok results -> head results
  Fail err -> error (show err)
  where
    -- Cache the grammar parser
    parser = EarleyParser.parse htmlGrammar

-- Function to pretty-print HtmlExp to an HTML Doc
prettyPrintHtml :: HtmlExp -> Doc ann
prettyPrintHtml = pprMode (flippr $ fromFunction <$> htmlPrettyPrinter)


-- Define a pretty-printer for HtmlExp that generates Markdown text
markdownPrettyPrinter :: (FliPprD arg exp) => FliPprM exp (A arg HtmlExp -> E exp D)
markdownPrettyPrinter = do
  -- Pretty-printer for names
  prettyName <- share $ \name -> case_ name [unName $ \str -> textAs str identifier]

  -- Use prettyName for variable-like structures
  let prettyVar = prettyName

  -- Define the pretty-printer logic for Markdown
  letrs [0] $ \prettyExp -> do
    def
      ( \_prec expr -> 
          case_ expr
            [ unContent $ \name -> prettyVar name
            -- I want to get rid of text "" but for some reason the terminal locks when i do that
            , unTagHtml $ \child -> text "" <+> prettyExp 0 child
            , unTagBold $ \child -> text "**" <+> prettyExp 0 child <+> text "**"
            , unTagH1 $ \child -> prettyExp 0 child <+> text "\n==="
            , unTagH2 $ \child -> prettyExp 0 child <+> text "\n---"
            , unTagH3 $ \child -> text "###" <+> prettyExp 0 child
            , unTagH4 $ \child -> text "####" <+> prettyExp 0 child
            , unTagH5 $ \child -> text "#####" <+> prettyExp 0 child
            , unTagP $ \child -> prettyExp 0 child <+> text "\n"
            , unTagDiv $ \child -> prettyExp 0 child <+> text "\n"
            --, unTagUl $ \children -> mconcat (map (\item -> text "- " <+> prettyExp 0 item <+> text "\n") children)
            , unTagLi $ \child -> text "- " <+> prettyExp 0 child
            , unSequence $ \first second -> text "<div>" <+> prettyExp 0 first <+> prettyExp 0 second <+> text "</div>"
            , otherwiseBranch $ parens . prettyExp 0
            ]
      )
      (return (prettyExp 0))

-- Define a grammar for parsing Markdown expressions
markdownGrammar :: (Grammar.GrammarD Char g) => g (Err ann HtmlExp)
markdownGrammar = parsingMode (flippr $ fromFunction <$> markdownPrettyPrinter)

-- Function to parse a Markdown string into an HtmlExp
parseMarkdown :: String -> HtmlExp
parseMarkdown input = case parser input of
  Ok results -> head results
  Fail err -> error (show err)
  where
    -- Cache the grammar parser
    parser = EarleyParser.parse markdownGrammar

-- Function to pretty-print HtmlExp to a Markdown Doc
prettyPrintMarkdown :: HtmlExp -> Doc ann
prettyPrintMarkdown = pprMode (flippr $ fromFunction <$> markdownPrettyPrinter)

-- Examples for testing the functionality

-- Example 1: Simple HTML structure <html>helloWorld</html>
htmlExample1 :: HtmlExp
htmlExample1 = TagHtml (Content (Name "helloWorld"))

-- Example 2: Nested HTML structure <html><b>helloWorld</b><h1>helloWorld</h1></html>
-- TODO: sequence doesn't work as expected
htmlExample2 :: HtmlExp
htmlExample2 = TagHtml (Sequence (TagBold (Content (Name "helloWorld"))) (TagH1 (Content (Name "helloWorld"))))

-- Example 3: Bold text <b>helloWorld</b>
htmlExample3 :: HtmlExp
htmlExample3 = TagBold (Content (Name "helloWorld"))

-- Example 4: Header level 3 <h3>helloWorld</h3>
htmlExample4 :: HtmlExp
htmlExample4 = TagH3 (Content (Name "helloWorld"))

htmlExample5 :: HtmlExp
htmlExample5 = TagP (Content (Name "thisisaparagraph."))

htmlExample6a :: HtmlExp
htmlExample6a = TagLi (Content (Name "Item 1"))

htmlExample6b :: HtmlExp
htmlExample6b = TagLi (Content (Name "Item 2"))

htmlExample7 :: HtmlExp
htmlExample7 = TagDiv (Sequence htmlExample6a htmlExample6b)

-- Main function to test examples
main :: IO ()
main = do
  let testExample example name = do
        putStrLn $ replicate 80 '='
        putStrLn $ "Testing Example: " ++ name
        putStrLn $ replicate 80 '='

        -- Pretty-print example as HTML
        let htmlDoc = show (prettyPrintHtml example)
        putStrLn $ "HTML Output: " ++ htmlDoc

        -- Parse the HTML output back to HtmlExp
        let parsedHtmlExp = parseHtml htmlDoc
        putStrLn $ "Parsed HtmlExp (from HTML): " ++ show parsedHtmlExp

        -- Verify that parsing round-trips correctly
        putStrLn $ "Round-Trip (HTML): " ++ show (parsedHtmlExp == example)

        putStrLn $ replicate 40 '-'

        -- Convert HtmlExp to Markdown
        let markdownDoc = show (prettyPrintMarkdown example)
        putStrLn $ "Markdown Output: " ++ markdownDoc

        -- Parse Markdown back to HtmlExp using parseMarkdown
        let parsedMarkdownExp = parseMarkdown markdownDoc
        putStrLn $ "Parsed HtmlExp (from Markdown): " ++ show parsedMarkdownExp

        -- Verify that Markdown parsing round-trips correctly
        putStrLn $ "Round-Trip (Markdown): " ++ show (parsedMarkdownExp == example)

        putStrLn ""

  -- Test each example
  testExample htmlExample1 "Example 1: <html>helloWorld</html>"
  testExample htmlExample2 "Example 2: <html><b>helloWorld</b><h1>helloWorld</h1></html>"
  testExample htmlExample3 "Example 3: <b>helloWorld</b>"
  testExample htmlExample4 "Example 4: <h3>helloWorld</h3>"
  testExample htmlExample5 "Example 5: <p>This is a Paragraph</p>"
  testExample htmlExample6a "Example 6a: <div><h1>Title</h1><ul><li>Item 1</li><li>Item 2</li></ul></div>"
  testExample htmlExample6b "Example 6b: <div><h1>Title</h1><ul><li>Item 1</li><li>Item 2</li></ul></div>"
  testExample htmlExample7 "Example 7: <div><h1>Title</h1><ul><li>Item 1</li><li>Item 2</li></ul></div>"





{-

-- Example HTML text for comparison
htmlTextDoc1 :: Doc ann
htmlTextDoc1 = text "<html> helloWorld </html>"

htmlTextDoc2 :: Doc ann
htmlTextDoc2 = text "<html> <b> helloWorld </b> </html>"

htmlTextDoc3 :: Doc ann
htmlTextDoc3 = text "<h1> helloWorld </h1>"




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