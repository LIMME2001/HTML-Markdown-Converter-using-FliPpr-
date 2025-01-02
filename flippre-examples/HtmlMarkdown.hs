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

import Prettyprinter (Doc)
import Text.Printf

newtype Name = Name String
  deriving (Eq, Show)

data Lit
  = LBool Bool
  | LInt Int
  deriving (Eq, Show)

data BinOp = Add | Mul
  deriving (Eq, Show)
data Exp
  = Op BinOp Exp Exp
  | Let Name Exp Exp
  | Literal Lit
  | If Exp Exp Exp
  | Var Name
  deriving (Eq, Show)

data Html
  = HtmlTag String [Html]
  | TextContent String
  deriving (Eq, Show)

$(mkUn ''Html)


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
keywords = ["true", "false", "let", "in", "if", "then", "else"]

textDFA :: AM.DFA Char
textDFA = AM.star (AM.range ' ' '~') -- Matches any printable character

flipprExp :: (FliPprD arg exp) => FliPprM exp (A arg Exp -> E exp D)
flipprExp = do
  pprName <- share $ \x -> case_ x [unName $ \s -> textAs s ident]
  pprInt <- share $ \n -> case_ n [atoiP $ \s -> textAs s numbers]
  pprBool <- share $ \b -> case_ b [unTrue $ text "true", unFalse $ text "false"]

  let pprLet p n e1 e2 =
        group $
          vsep
            [ hsep [text "let", pprName n <+>. text "=" <+>. align (p 0 e1)]
            , hsep [text "in", align (p 0 e2)]
            ]
  let pprIf p e0 e1 e2 =
        group $
          vsep
            [ hsep [text "if", p 0 e0]
            , hsep [text "then", p 0 e1]
            , hsep [text "else", p 0 e2]
            ]
  let pprOp p (Fixity assoc prec) opS e1 e2 =
        let (dl, dr)
              | AssocL <- assoc = (0, 1)
              | AssocR <- assoc = (1, 0)
              | AssocN <- assoc = (1, 1)
        in  p (prec + dl) e1 <+>. text opS <+>. p (prec + dr) e2

  let pprVar = pprName
  let pprLit l =
        case_
          l
          [ unLBool pprBool
          , unLInt pprInt
          ]

  let pprHtmlTag p tag children =
        case_ tag
          [ Branch
              (PartialBij "htmlTag" Just Just)
              (\t -> group $
                  vsep
                    [ text "<" <> textAs t textDFA <> text ">"
                    , indent 2 $ foldMap p children
                    , text "</" <> textAs t textDFA <> text ">"
                    ])
          ]

  let pprTextContent =
        share $ \txt -> case_ txt
          [ Branch
              (PartialBij "textContent" Just Just)
              (\s -> textAs s textDFA)
          ]


  -- Technique mentioned in http://www.haskellforall.com/2020/11/pretty-print-syntax-trees-with-this-one.html.
  -- A trick is that patterns are intentionally overlapping, so that it can parse ugly string, wihtout <?
  letrs [0 .. 3] $ \pExp ->
    def
      ( \prec x ->
          if
            | prec == 0 ->
                case_
                  x
                  [ unLet $ \n e1 e2 -> pprLet pExp n e1 e2
                  , unIf $ \e0 e1 e2 -> pprIf pExp e0 e1 e2
                  , otherwiseP $ pExp (prec + 1)
                  ]
            | prec == 1 ->
                case_
                  x
                  [ $(pat 'Op) $(pat 'Add) varP varP `br` \e1 e2 -> pprOp pExp (Fixity AssocL 1) "+" e1 e2
                  , otherwiseP $ pExp (prec + 1)
                  ]
            | prec == 2 ->
                case_
                  x
                  [ --  $(branch [p| Op Mul e1 e2 |] [| pprOp pExp (Fixity AssocL 2)  "*" e1 e2 |]),
                    -- compostional approach to pattern matching
                    br ($(pat 'Op) $(pat 'Mul) varP varP) $ \e1 e2 -> pprOp pExp (Fixity AssocL 2) "*" e1 e2
                  , otherwiseP $ pExp (prec + 1)
                  ]
            | otherwise ->
                case_
                  x
                  [ unVar $ \n -> pprVar n
                  , unLiteral $ \l -> pprLit l
                  , unHtmlTag $ \tag children -> pprHtmlTag pExp tag children
                  , unTextContent pprTextContent
                  , otherwiseP $ parens . pExp 0
                  ]
      )
      $ return (pExp 0)

gExp :: (G.GrammarD Char g) => g (Err ann Exp)
gExp = parsingModeWith (CommentSpec (Just "--") (Just $ BlockCommentSpec "{-" "-}" True)) (flippr $ fromFunction <$> flipprExp)

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
  Let x (Op Mul (Literal $ LInt 5) (Op Add (Literal $ LInt 3) (Literal $ LInt 4))) $
    Let y (If (Literal $ LBool True) (Literal $ LBool False) (Literal $ LBool False)) $
      If (Var y) (Var x) (Literal $ LInt 0)
  where
    x = Name "x"
    y = Name "y"

main :: IO ()
main = do
  -- Test with a complex expression (existing test)
  let s = show (pprExp exp1)
  putStrLn "`pprExp exp1` results in ..."
  putStrLn s
  let e = parseExp s
  putStrLn $ replicate 80 '-'
  putStrLn "`parseExp (pprExp exp1)` results in ..."
  print e
  putStrLn $ replicate 80 '-'
  printf "`exp1 == parseExp (pprExp exp1)` = %s\n" (show $ e == exp1)

  -- Test with a single word (new test)
  let singleWordExp = Var (Name "testVariable")
  let singleWordPrinted = show (pprExp singleWordExp)
  putStrLn $ replicate 80 '-'
  putStrLn "`pprExp (Var (Name \"testVariable\"))` results in ..."
  putStrLn singleWordPrinted
  let singleWordParsed = parseExp singleWordPrinted
  putStrLn $ replicate 80 '-'
  putStrLn "`parseExp (pprExp (Var (Name \"testVariable\")))` results in ..."
  print singleWordParsed
  putStrLn $ replicate 80 '-'
  printf "`Var (Name \"testVariable\") == parseExp (pprExp (Var (Name \"testVariable\")))` = %s\n"
         (show $ singleWordParsed == singleWordExp)











{-
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.FliPpr
import qualified Text.FliPpr.Automaton as AM
import qualified Text.FliPpr.Grammar.Driver.Earley as E
import Prelude

-- ___ Data Definitions ___

data Content
  = Bold String -- Bold text like "**bold**" or "<b>bold</b>"
  deriving (Eq, Show)

$(mkUn ''Content)

-- ___ DFA Definitions ___

-- DFA for identifiers (a-z, A-Z, 0-9, and underscores)
ident :: AM.DFA Char
ident = AM.range 'a' 'z' <> AM.range 'A' 'Z' <> AM.range '0' '9' <> AM.singleton '_'

-- ___ Pretty-Printer for Markdown ↔ HTML ___

flipprContent :: (FliPprD arg exp) => FliPprM exp (A arg Content -> E exp D)
flipprContent = do
  -- Bold Pretty-Printer
  pprBold <- define $ \x ->
    case_ x
      [ unBold $ \txt -> text "**" <> textAs txt ident <> text "**",
        unBold $ \txt -> text "<b>" <> textAs txt ident <> text "</b>"
      ]

  -- Return only the Bold Pretty-Printer
  return $ fromFunction pprBold

-- ___ Helper Functions for Pretty-Printing and Parsing ___

pprContent :: Content -> String
pprContent = show . pprMode (flippr $ fromFunction <$> flipprContent)

parseContent :: String -> Either String [Content]
parseContent input =
  case E.parse (parsingMode $ flippr $ fromFunction <$> flipprContent) input of
    Ok result -> Right result
    Fail err -> Left (show err)

-- ___ Main Program for Testing ___

main :: IO ()
main = do
  let markdownExample = "**bold text**"
  let htmlExample = "<b>bold text</b>"

  putStrLn "Parsing Markdown to HTML:"
  case parseContent markdownExample of
    Right content -> do
      putStrLn "Parsed content:"
      print content
      putStrLn "Pretty-printed HTML:"
      mapM_ (putStrLn . pprContent) content
    Left err -> putStrLn $ "Error: " ++ err

  putStrLn $ replicate 80 '-'

  putStrLn "Parsing HTML to Markdown:"
  case parseContent htmlExample of
    Right content -> do
      putStrLn "Parsed content:"
      print content
      putStrLn "Pretty-printed Markdown:"
      mapM_ (putStrLn . pprContent) content
    Left err -> putStrLn $ "Error: " ++ err
-}