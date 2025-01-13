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

import System.CPUTime
import Control.DeepSeq

import Data.String (fromString)
import Data.Char (isAlphaNum)

import Prettyprinter (Doc)
import Text.Printf

import Data.List (isPrefixOf, isSuffixOf)

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

identifier :: Automaton.DFA Char
identifier = (Automaton.star allowedChars) `Automaton.difference` Automaton.unions (map fromString reservedKeywords)
  where
    allowedChars = Automaton.unions 
      [ Automaton.range 'a' 'z'
      , Automaton.range 'A' 'Z'
      , Automaton.range '0' '9'
      , Automaton.singleton '_'
      , Automaton.singleton ' '
      , Automaton.singleton '.'
      , Automaton.singleton '!'
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
            , unSequence $ \first second -> prettyExp 0 first <+> prettyExp 0 second
            , otherwiseBranch $ parens . prettyExp 0
            ]
      )
      (return (prettyExp 0))

-- Define a grammar for parsing HTML expressions
htmlGrammar :: (Grammar.GrammarD Char g) => g (Err ann HtmlExp)
htmlGrammar = parsingMode (flippr $ fromFunction <$> htmlPrettyPrinter)

-- Function to parse an HTML string into an HtmlExp (remove HTML wrapper)
parseHtml :: String -> HtmlExp
parseHtml input = case parser (stripHtmlTags input) of
  Ok results -> normalizeHtmlExpWithLimit 100 (head results) -- Use a limit of 100
  Fail err -> error (show err)
  where
    parser = EarleyParser.parse htmlGrammar

    
stripHtmlTags :: String -> String
stripHtmlTags = trim . removePrefixSuffix "<html>" "</html>"

trim :: String -> String
trim = unwords . words

removePrefixSuffix :: String -> String -> String -> String
removePrefixSuffix prefix suffix str =
  let str' = if prefix `isPrefixOf` str then drop (length prefix) str else str
  in if suffix `isSuffixOf` str' then take (length str' - length suffix) str' else str'
    

-- Function to pretty-print HtmlExp to an HTML Doc
prettyPrintHtml :: HtmlExp -> Doc ann
prettyPrintHtml expr =
  let normalizedExpr = normalizeHtmlExpWithLimit 100 expr -- Apply normalization with a limit
  in text "<html>" <+> pprMode (flippr $ fromFunction <$> htmlPrettyPrinter) normalizedExpr <+> text "</html>"

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


{-
-- Function to normalize seuqnces that only contains context
normalizeHtmlExp :: HtmlExp -> HtmlExp
-- Merge adjacent Content nodes in Sequence
normalizeHtmlExp (Sequence (Content (Name str1)) (Content (Name str2))) =
  Content (Name (str1 ++ " " ++ str2))
-- Recursively normalize left and right in Sequence
normalizeHtmlExp (Sequence left right) =
  case (normalizeHtmlExp left, normalizeHtmlExp right) of
    (Content (Name str1), Content (Name str2)) -> Content (Name (str1 ++ " " ++ str2))
    (normalizedLeft, normalizedRight) -> Sequence normalizedLeft normalizedRight
-- Recursively normalize children in all tags
normalizeHtmlExp (TagH1 child) = TagH1 (normalizeHtmlExp child)
normalizeHtmlExp (TagH2 child) = TagH2 (normalizeHtmlExp child)
normalizeHtmlExp (TagH3 child) = TagH3 (normalizeHtmlExp child)
normalizeHtmlExp (TagH4 child) = TagH4 (normalizeHtmlExp child)
normalizeHtmlExp (TagH5 child) = TagH5 (normalizeHtmlExp child)
normalizeHtmlExp (TagBold child) = TagBold (normalizeHtmlExp child)
normalizeHtmlExp (TagP child) = TagP (normalizeHtmlExp child)
normalizeHtmlExp (TagDiv child) = TagDiv (normalizeHtmlExp child)
normalizeHtmlExp (TagLi child) = TagLi (normalizeHtmlExp child)
-- Leave plain Content nodes as-is
normalizeHtmlExp content@(Content _) = content
-}

normalizeHtmlExpWithLimit :: Int -> HtmlExp -> HtmlExp
normalizeHtmlExpWithLimit 0 expr = expr -- Stop normalization at depth limit
normalizeHtmlExpWithLimit depth expr = case expr of
  Sequence left right ->
    let normalizedLeft = normalizeHtmlExpWithLimit (depth - 1) left
        normalizedRight = normalizeHtmlExpWithLimit (depth - 1) right
    in case (normalizedLeft, normalizedRight) of
         (Content (Name str1), Content (Name str2)) -> Content (Name (str1 ++ " " ++ str2))
         _ -> Sequence normalizedLeft normalizedRight
  TagH1 child -> TagH1 (normalizeHtmlExpWithLimit (depth - 1) child)
  TagH2 child -> TagH2 (normalizeHtmlExpWithLimit (depth - 1) child)
  TagH3 child -> TagH3 (normalizeHtmlExpWithLimit (depth - 1) child)
  TagH4 child -> TagH4 (normalizeHtmlExpWithLimit (depth - 1) child)
  TagH5 child -> TagH5 (normalizeHtmlExpWithLimit (depth - 1) child)
  TagBold child -> TagBold (normalizeHtmlExpWithLimit (depth - 1) child)
  TagP child -> TagP (normalizeHtmlExpWithLimit (depth - 1) child)
  TagDiv child -> TagDiv (normalizeHtmlExpWithLimit (depth - 1) child)
  TagLi child -> TagLi (normalizeHtmlExpWithLimit (depth - 1) child)
  -- Repeat for all tag types...
  Content name -> Content name


-- Examples for testing the functionality

-- Example 1: Simple HTML structure <html>helloWorld</html>
htmlExample1 :: HtmlExp
htmlExample1 = Content (Name "helloWorld")

-- Example 2: Nested HTML structure <html><b>helloWorld</b><h1>helloWorld</h1></html>
-- TODO: sequence doesn't work as expected
htmlExample2 :: HtmlExp
htmlExample2 = Sequence (TagBold (Content (Name "hello World"))) (TagH1 (Content (Name "hello World")))

-- Example 3: Bold text <b>helloWorld</b>
htmlExample3 :: HtmlExp
htmlExample3 = TagBold (Content (Name "hello World. I would like to test the following test hejsan h"))

-- Example 4: Header level 3 <h3>helloWorld</h3>
-- Problem when made into two parts
htmlExample4 :: HtmlExp
htmlExample4 = TagH3 (Content (Name "hello World"))

htmlExample5 :: HtmlExp
htmlExample5 = TagP (Content (Name "this is a paragraph."))

htmlExample6a :: HtmlExp
htmlExample6a = TagLi (Content (Name "Item 1"))

htmlExample6b :: HtmlExp
htmlExample6b = TagLi (Content (Name "Item 2"))

htmlExample7 :: HtmlExp
htmlExample7 = TagDiv (Sequence htmlExample6a htmlExample6b)

-- Add instances for NFData
instance NFData Name where
  rnf (Name str) = rnf str

instance NFData Lit where
  rnf (LString str) = rnf str

instance NFData HtmlExp where
  rnf (Content name) = rnf name
  rnf (TagBold child) = rnf child
  rnf (TagH1 child) = rnf child
  rnf (TagH2 child) = rnf child
  rnf (TagH3 child) = rnf child
  rnf (TagH4 child) = rnf child
  rnf (TagH5 child) = rnf child
  rnf (TagP child) = rnf child
  rnf (TagDiv child) = rnf child
  rnf (TagLi child) = rnf child
  rnf (Sequence left right) = rnf left `seq` rnf right

-- Update countTime to require NFData
countTime :: NFData a => String -> IO a -> IO a
countTime str comp = do
  s <- getCPUTime
  r <- comp
  rnf r `seq` return () -- Ensure computation is fully evaluated
  e <- getCPUTime
  let d = fromIntegral (e - s) / (10 ^ 9) -- Convert to milliseconds
  putStrLn $ "Elapsed: " ++ show d ++ " ms."
  return r

-- Main function to test examples
main :: IO ()
main = do
  let testExample example name = do
        putStrLn $ replicate 80 '='
        putStrLn $ "Testing Example: " ++ name
        putStrLn $ replicate 80 '='

        -- Pretty-print example as HTML
        htmlDoc <- countTime "HTML Output" $ do
          let htmlDoc = show (prettyPrintHtml example)
          putStrLn $ "HTML Output: " ++ htmlDoc
          return htmlDoc

        -- Parse the HTML output back to HtmlExp
        parsedHtmlExp <- countTime "Parsed HtmlExp (from HTML)" $ do
          let parsedHtmlExp = parseHtml htmlDoc
          putStrLn $ "Parsed HtmlExp (from HTML): " ++ show parsedHtmlExp
          return parsedHtmlExp

        -- Verify that parsing round-trips correctly
        putStrLn $ "Round-Trip (HTML): " ++ show (parsedHtmlExp == example)

        putStrLn $ replicate 40 '-'

        -- Convert HtmlExp to Markdown
        markdownDoc <- countTime "Markdown Output" $ do
          let markdownDoc = show (prettyPrintMarkdown example)
          putStrLn $ "Markdown Output: " ++ markdownDoc
          return markdownDoc

        -- Parse Markdown back to HtmlExp using parseMarkdown
        parsedMarkdownExp <- countTime "Parsed HtmlExp (from Markdown)" $ do
          let parsedMarkdownExp = parseMarkdown markdownDoc
          putStrLn $ "Parsed HtmlExp (from Markdown): " ++ show parsedMarkdownExp
          return parsedMarkdownExp

        -- Verify that Markdown parsing round-trips correctly
        putStrLn $ "Round-Trip (Markdown): " ++ show (parsedMarkdownExp == example)

        putStrLn ""

  -- Test each example
  testExample htmlExample1 "Example 1: <html>helloWorld</html>"
  testExample htmlExample2 "Example 2: <html><b>helloWorld</b><h1>helloWorld</h1></html>"
  testExample htmlExample3 "Example 3: <b>helloWorld</b>"
  testExample htmlExample4 "Example 4: <h3>helloWorld</h3>"
  testExample htmlExample5 "Example 5: <p>This is a Paragraph</p>"
  testExample htmlExample6a "Example 6a: <li>Item 1</li>"
  testExample htmlExample6b "Example 6b: <li>Item 2</li>"
  testExample htmlExample7 "Example 7: <div><li>Item 1</li><li>Item 2</li></div>"
