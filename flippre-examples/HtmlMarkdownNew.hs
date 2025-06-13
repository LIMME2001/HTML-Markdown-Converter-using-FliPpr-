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
{-# LANGUAGE QualifiedDo #-} -- use this as it's easier
{-# LANGUAGE RecursiveDo #-} -- should be used together with qualifieddo
--rebindable syntax can be difficult to use.

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


-- Simplified AST structure based on Markdown
data HtmlExp
  = Content String           -- Plain text content
  | Elem Tag [HtmlExp]       -- Element with tag and children
  deriving (Eq, Show)

data Tag
  = TagBold
  | TagH1
  | TagH2
  | TagH3
  | TagH4
  | TagH5
  | TagP
  | TagDiv
  | TagLi
  deriving (Eq, Show)

-- Generate partial bijections
$(mkUn ''Tag)
$(mkUn ''HtmlExp)

-- Helper function for "otherwise" branches
otherwiseBranch :: (arg HtmlExp -> exp t) -> Branch arg exp HtmlExp t
otherwiseBranch = Branch (PartialBij "otherwiseBranch" Just Just)

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
    


-- TIPS
-- Use rec inside the function.
-- Have to disallow the concatination of the
-- print undefined for unwanted structures

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
            , unSequence $ \first second -> prettyExp 0 first <+> prettyExp 0 second --this is the time theif for sequencing (when parsing HTML)...
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
  let normalizedExpr = expr -- Apply normalization with a limit
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
            , unTagP $ \child -> prettyExp 0 child <+> text "\n\n"
            , unTagDiv $ \child -> prettyExp 0 child <+> text "\n\n"
            --, unTagUl $ \children -> mconcat (map (\item -> text "- " <+> prettyExp 0 item <+> text "\n") children)
            , unTagLi $ \child -> text "- " <+> prettyExp 0 child <+> text "\n"
            , unSequence $ \first second -> prettyExp 0 first <+> text "\n\n" <+> prettyExp 0 second --this becomes the time theif for sequencing (when parsing Markdown) if implemented without div... Atm highly questionable at best
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
  Ok results -> normalizeHtmlExpWithLimit 100 (head results)
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
normalizeHtmlExpWithLimit depth expr =
  case expr of
    -- Merge adjacent Content nodes into one
    Sequence (Content (Name str1)) (Content (Name str2)) ->
      Content (Name (str1 ++ " " ++ str2))
    -- Recursively normalize sequences
    Sequence left right ->
      let normalizedLeft = normalizeHtmlExpWithLimit (depth - 1) left
          normalizedRight = normalizeHtmlExpWithLimit (depth - 1) right
      in case (normalizedLeft, normalizedRight) of
            (Content (Name s1), Content (Name s2)) -> Content (Name (s1 ++ " " ++ s2))
            _ -> Sequence normalizedLeft normalizedRight
    -- Normalize nested list elements
    TagH1 child -> TagH1 (normalizeHtmlExpWithLimit (depth - 1) child)
    TagH2 child -> TagH2 (normalizeHtmlExpWithLimit (depth - 1) child)
    TagH3 child -> TagH3 (normalizeHtmlExpWithLimit (depth - 1) child)
    TagH4 child -> TagH4 (normalizeHtmlExpWithLimit (depth - 1) child)
    TagH5 child -> TagH5 (normalizeHtmlExpWithLimit (depth - 1) child)
    TagBold child -> TagBold (normalizeHtmlExpWithLimit (depth - 1) child)
    TagP child -> TagP (normalizeHtmlExpWithLimit (depth - 1) child)
    TagDiv child -> TagDiv (normalizeHtmlExpWithLimit (depth - 1) child)
    TagLi child -> TagLi (normalizeHtmlExpWithLimit (depth - 1) child)
    -- Keep content unchanged
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
  testExample htmlExample1 "Example 1: <html> helloWorld </html>"
  testExample htmlExample2 "Example 2: <html> <b> helloWorld </b> <h1> helloWorld </h1> </html>"
  testExample htmlExample3 "Example 3: <b> hello World. I would like to test the following test hejsan h </b>"
  testExample htmlExample4 "Example 4: <h3> hello World </h3>"
  testExample htmlExample5 "Example 5: <p> This is a paragraph </p>"
  testExample htmlExample6a "Example 6a: <li> Item 1 </li>"
  testExample htmlExample6b "Example 6b: <li> Item 2 </li>"
  testExample htmlExample7 "Example 7: <div> <li> Item 1 </li> <li> Item 2 </li> </div>"

  {-
================================================================================
Testing Example: Example 1: <html> helloWorld </html>
================================================================================
HTML Output: <html> helloWorld </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): Content (Name "helloWorld")
Elapsed: 15.625 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: helloWorld
Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): Content (Name "helloWorld")
Elapsed: 0.0 ms.
Round-Trip (Markdown): True

================================================================================
Testing Example: Example 2: <html> <b> helloWorld </b> <h1> helloWorld </h1> </html>
================================================================================
HTML Output: <html> <b> hello World </b> <h1> hello World </h1> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): Sequence (TagBold (Content (Name "hello World"))) (TagH1 (Content (Name "hello World")))
Elapsed: 0.0 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: ** hello World **

 hello World
===
Elapsed: 15.625 ms.
Parsed HtmlExp (from Markdown): Sequence (TagBold (Content (Name "hello World"))) (TagH1 (Content (Name "hello World")))
Elapsed: 0.0 ms.
Round-Trip (Markdown): True

================================================================================
Testing Example: Example 3: <b> hello World. I would like to test the following test hejsan h </b>
================================================================================
HTML Output: <html> <b> hello World. I would like to test the following test hejsan h </b> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): TagBold (Content (Name "hello World. I would like to test the following test hejsan h"))
Elapsed: 92453.125 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: ** hello World. I would like to test the following test hejsan h **
Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): TagBold (Content (Name "hello World. I would like to test the following test hejsan h"))
Elapsed: 0.0 ms.
Round-Trip (Markdown): True

================================================================================
Testing Example: Example 4: <h3> hello World </h3>
================================================================================
HTML Output: <html> <h3> hello World </h3> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): TagH3 (Content (Name "hello World"))
Elapsed: 0.0 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: ### hello World
Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): TagH3 (Content (Name "hello World"))
Elapsed: 0.0 ms.
Round-Trip (Markdown): True

================================================================================
Testing Example: Example 5: <p> This is a paragraph </p>
================================================================================
HTML Output: <html> <p> this is a paragraph. </p> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): TagP (Content (Name "this is a paragraph."))
Elapsed: 0.0 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: this is a paragraph.


Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): TagDiv (Content (Name "this is a paragraph."))
Elapsed: 0.0 ms.
Round-Trip (Markdown): False

================================================================================
Testing Example: Example 6a: <li> Item 1 </li>
================================================================================
HTML Output: <html> <li> Item 1 </li> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): TagLi (Content (Name "Item 1"))
Elapsed: 0.0 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: -  Item 1

Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): TagLi (Content (Name "Item 1"))
Elapsed: 0.0 ms.
Round-Trip (Markdown): True

================================================================================
Testing Example: Example 6b: <li> Item 2 </li>
================================================================================
HTML Output: <html> <li> Item 2 </li> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): TagLi (Content (Name "Item 2"))
Elapsed: 0.0 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: -  Item 2

Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): TagLi (Content (Name "Item 2"))
Elapsed: 0.0 ms.
Round-Trip (Markdown): True

================================================================================
Testing Example: Example 7: <div> <li> Item 1 </li> <li> Item 2 </li> </div>
================================================================================
HTML Output: <html> <div> <li> Item 1 </li> <li> Item 2 </li> </div> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): TagDiv (Sequence (TagLi (Content (Name "Item 1"))) (TagLi (Content (Name "Item 2"))))
Elapsed: 0.0 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: -  Item 1


 -  Item 2



Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): TagLi (Sequence (Content (Name "Item 1")) (TagLi (Content (Name "Item 2"))))
Elapsed: 15.625 ms.
Round-Trip (Markdown): False




  RESULTS NO DIV IN MARKDOWN NO NORMALIZATION
  ================================================================================
Testing Example: Example 1: <html>helloWorld</html>
================================================================================
HTML Output: <html> helloWorld </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): Content (Name "helloWorld")
Elapsed: 0.0 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: helloWorld
Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): Content (Name "helloWorld")
Elapsed: 0.0 ms.
Round-Trip (Markdown): True

================================================================================
Testing Example: Example 2: <html><b>helloWorld</b><h1>helloWorld</h1></html>
================================================================================
HTML Output: <html> <b> hello World </b> <h1> hello World </h1> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): Sequence (TagBold (Content (Name "hello World"))) (TagH1 (Sequence (Content (Name "hello")) (Content (Name "World"))))
Elapsed: 0.0 ms.
Round-Trip (HTML): False
----------------------------------------
Markdown Output: ** hello World ** hello World
===
Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): TagH1 (Sequence (TagBold (Content (Name "hello World"))) (Sequence (Content (Name "hello")) (Content (Name "World"))))
Elapsed: 0.0 ms.
Round-Trip (Markdown): False

================================================================================
Testing Example: Example 3: <b>helloWorld</b>
================================================================================
HTML Output: <html> <b> hello World. I would like to test the following test hejsan h </b> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): TagBold (Sequence (Sequence (Sequence (Sequence (Sequence (Content (Name "hello World. I")) (Content (Name "would"))) (Sequence (Content (Name "like")) (Content (Name "to")))) (Sequence (Content (Name "test")) (Content (Name "the")))) (Sequence (Content (Name "following")) (Content (Name "test")))) (Content (Name "hejsan h")))
Elapsed: 28000.0 ms.
Round-Trip (HTML): False
----------------------------------------
Markdown Output: ** hello World. I would like to test the following test hejsan h **
Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): TagBold (Sequence (Sequence (Sequence (Sequence (Sequence (Content (Name "hello World. I")) (Content (Name "would"))) (Sequence (Content (Name "like")) (Content (Name "to")))) (Sequence (Content (Name "test")) (Content (Name "the")))) (Sequence (Content (Name "following")) (Content (Name "test")))) (Content (Name "hejsan h")))
Elapsed: 28156.25 ms.
Round-Trip (Markdown): False

================================================================================
Testing Example: Example 4: <h3>helloWorld</h3>
================================================================================
HTML Output: <html> <h3> hello World </h3> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): TagH3 (Sequence (Content (Name "hello")) (Content (Name "World")))
Elapsed: 0.0 ms.
Round-Trip (HTML): False
----------------------------------------
Markdown Output: ### hello World
Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): TagH3 (Sequence (Content (Name "hello")) (Content (Name "World")))
Elapsed: 0.0 ms.
Round-Trip (Markdown): False

================================================================================
Testing Example: Example 5: <p>This is a Paragraph</p>
================================================================================
HTML Output: <html> <p> this is a paragraph. </p> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): TagP (Content (Name "this is a paragraph."))
Elapsed: 0.0 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: this is a paragraph.

Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): Sequence (Content (Name "this is a paragraph.")) (Content (Name ""))
Elapsed: 0.0 ms.
Round-Trip (Markdown): False

================================================================================
Testing Example: Example 6a: <li>Item 1</li>
================================================================================
HTML Output: <html> <li> Item 1 </li> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): TagLi (Content (Name "Item 1"))
Elapsed: 0.0 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: -  Item 1
Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): TagLi (Content (Name "Item 1"))
Elapsed: 0.0 ms.
Round-Trip (Markdown): True

================================================================================
Testing Example: Example 6b: <li>Item 2</li>
================================================================================
HTML Output: <html> <li> Item 2 </li> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): TagLi (Content (Name "Item 2"))
Elapsed: 0.0 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: -  Item 2
Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): TagLi (Content (Name "Item 2"))
Elapsed: 0.0 ms.
Round-Trip (Markdown): True

================================================================================
Testing Example: Example 7: <div><li>Item 1</li><li>Item 2</li></div>
================================================================================
HTML Output: <html> <div> <li> Item 1 </li> <li> Item 2 </li> </div> </html>
Elapsed: 0.0 ms.
Parsed HtmlExp (from HTML): TagDiv (Sequence (TagLi (Content (Name "Item 1"))) (TagLi (Content (Name "Item 2"))))
Elapsed: 0.0 ms.
Round-Trip (HTML): True
----------------------------------------
Markdown Output: -  Item 1 -  Item 2

Elapsed: 0.0 ms.
Parsed HtmlExp (from Markdown): Sequence (Sequence (Sequence (TagLi (Content (Name "Item"))) (Sequence (Content (Name "1")) (TagLi (Content (Name "Item"))))) (Content (Name "2"))) (Content (Name ""))
Elapsed: 0.0 ms.
Round-Trip (Markdown): False
  -}
















{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecursiveDo #-}

import Text.FliPpr
import qualified Text.FliPpr.Automaton as Automaton
import qualified Text.FliPpr.Grammar as Grammar
import qualified Text.FliPpr.Grammar.Driver.Earley as EarleyParser
import Text.FliPpr.QualifiedDo as FliPpr
import Text.FliPpr.RecursiveDo as FliPpr

import System.CPUTime
import Control.DeepSeq

import Data.String (fromString)
import Data.Char (isAlphaNum)

import Prettyprinter (Doc)
import Text.Printf

import Data.List (isPrefixOf, isSuffixOf)






-- HTML pretty printer
htmlPrettyPrinter :: (FliPprD arg exp) => FliPprM exp (A arg HtmlExp -> E exp D)
htmlPrettyPrinter = FliPpr.recursiveDo $ do
  prettyExp <- FliPpr.mdo
    let
      elemPrinter tag open close children = 
        text open <+> childrenPrinter children <+> text close
        where
          childrenPrinter = case_ children
            [ unContent $ \str -> text str
            , unElem $ \tag' children' -> 
                if tag' == tag
                then childrenPrinter children'
                else prettyExp (Elem tag' children')
            ]
    
    def $ \_prec expr -> case_ expr
      [ unContent $ \str -> text str
      , unElem $ \tag children -> case tag of
          TagBold -> elemPrinter TagBold "<b>" "</b>" children
          TagH1 -> elemPrinter TagH1 "<h1>" "</h1>" children
          TagH2 -> elemPrinter TagH2 "<h2>" "</h2>" children
          TagH3 -> elemPrinter TagH3 "<h3>" "</h3>" children
          TagH4 -> elemPrinter TagH4 "<h4>" "</h4>" children
          TagH5 -> elemPrinter TagH5 "<h5>" "</h5>" children
          TagP -> elemPrinter TagP "<p>" "</p>" children
          TagDiv -> elemPrinter TagDiv "<div>" "</div>" children
          TagLi -> elemPrinter TagLi "<li>" "</li>" children
      , otherwiseBranch $ parens . prettyExp
      ]
    return prettyExp

-- HTML grammar keep
htmlGrammar :: (Grammar.GrammarD Char g) => g (Err ann HtmlExp)
htmlGrammar = parsingMode (flippr $ fromFunction <$> htmlPrettyPrinter)

-- Markdown pretty printer
markdownPrettyPrinter :: (FliPprD arg exp) => FliPprM exp (A arg HtmlExp -> E exp D)
markdownPrettyPrinter = FliPpr.recursiveDo $ do
  prettyExp <- FliPpr.mdo
    def $ \_prec expr -> case_ expr
      [ unContent $ \str -> text str
      , unElem $ \tag children -> case tag of
          TagBold -> text "**" <+> prettyExp children <+> text "**"
          TagH1 -> prettyExp children <+> text "\n==="
          TagH2 -> prettyExp children <+> text "\n---"
          TagH3 -> text "###" <+> prettyExp children
          TagH4 -> text "####" <+> prettyExp children
          TagH5 -> text "#####" <+> prettyExp children
          TagP -> prettyExp children <+> text "\n\n"
          TagDiv -> prettyExp children <+> text "\n\n"
          TagLi -> text "- " <+> prettyExp children <+> text "\n"
      , otherwiseBranch $ parens . prettyExp
      ]
    return prettyExp

-- Markdown grammar
markdownGrammar :: (Grammar.GrammarD Char g) => g (Err ann HtmlExp)
markdownGrammar = parsingMode (flippr $ fromFunction <$> markdownPrettyPrinter)

-- Helper functions for parsing
stripHtmlTags :: String -> String
stripHtmlTags = trim . removePrefixSuffix "<html>" "</html>"

trim :: String -> String
trim = unwords . words

removePrefixSuffix :: String -> String -> String -> String
removePrefixSuffix prefix suffix str =
  let str' = if prefix `isPrefixOf` str then drop (length prefix) str else str
  in if suffix `isSuffixOf` str' then take (length str' - length suffix) str' else str'

-- Parsing functions
parseHtml :: String -> HtmlExp
parseHtml input = case EarleyParser.parse htmlGrammar (stripHtmlTags input) of
  Ok results -> head results
  Fail err -> error (show err)

parseMarkdown :: String -> HtmlExp
parseMarkdown input = case EarleyParser.parse markdownGrammar input of
  Ok results -> head results
  Fail err -> error (show err)

-- Pretty printing functions
prettyPrintHtml :: HtmlExp -> Doc ann
prettyPrintHtml expr = text "<html>" <+> pprMode (flippr $ fromFunction <$> htmlPrettyPrinter) expr <+> text "</html>"

prettyPrintMarkdown :: HtmlExp -> Doc ann
prettyPrintMarkdown = pprMode (flippr $ fromFunction <$> markdownPrettyPrinter)
