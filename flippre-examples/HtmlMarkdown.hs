{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}

import Text.FliPpr
import qualified Text.FliPpr.Automaton as AM
import qualified Text.FliPpr.Grammar as G
import qualified Text.FliPpr.Grammar.Driver.Earley as E
import qualified Text.FliPpr.QDo as F

import Data.String (fromString)
import Debug.Trace (trace)
import qualified Prettyprinter as PP (Doc)
import Data.List (isPrefixOf, isSuffixOf)

-- New data type structures compared to before
data Tag = Bold | H1 | H2 | H3 | H4 | H5 | P | Div | Li | Ul | Ol
    deriving stock (Eq, Show)

data Doc
    = Text String
    | Element Tag [Doc]
    | Sequence [Doc]
    deriving stock (Eq, Show)

$(mkUn ''Tag)
$(mkUn ''Doc)

plainText :: AM.DFA Char
plainText = AM.star (AM.unions [
    AM.range 'a' 'z',
    AM.range 'A' 'Z',
    AM.range '0' '9',
    AM.singleton ' ',
    AM.singleton '.',
    AM.singleton '!',
    AM.singleton '?',
    AM.singleton ',',
    AM.singleton ':',
    AM.singleton ';'
    ])

-- Markdown pretty printer
pprMarkdown :: (FliPprD arg exp) => FliPprM exp (A arg Doc -> E exp D)
pprMarkdown = F.do
    rec pDoc <- share $ \doc ->
            case_ doc
                [ unText $ \str -> textAs str plainText
                , unElement $ \tag children ->
                    case_ tag
                        [ unBold $ text "**" <> pDocList children <> text "**"
                        , unH1 $ pDocList children <> text "\n" <> text (replicate 20 '=')
                        , unH2 $ pDocList children <> text "\n" <> text (replicate 20 '-')
                        , unH3 $ text "### " <> pDocList children
                        , unH4 $ text "#### " <> pDocList children
                        , unH5 $ text "##### " <> pDocList children
                        , unP $ pDocList children <> text "\n\n"
                        , unDiv $ pDocList children <> text "\n"
                        , unLi $ text "- " <> pDocList children <> text "\n"
                        , unUl $ pDocList children
                        , unOl $ pDocList children
                        ]
                , unSequence $ pDocList
                ]

        pDocList <- share $ \docs ->
            case_ docs
                [ unNil $ text ""
                , unCons $ \head tail ->
                    pDoc head <> pDocList tail
                ]

    pure pDoc

-- HTML pretty printer
pprHTML :: (FliPprD arg exp) => FliPprM exp (A arg Doc -> E exp D)
pprHTML = F.do
    pprTag <- share $ \tag ->
        case_ tag
            [ unBold $ text "b"
            , unH1 $ text "h1"
            , unH2 $ text "h2"
            , unH3 $ text "h3"
            , unH4 $ text "h4"
            , unH5 $ text "h5"
            , unP $ text "p"
            , unDiv $ text "div"
            , unLi $ text "li"
            , unUl $ text "ul"
            , unOl $ text "ol"
            ]

    rec pDoc <- share $ \doc ->
            case_ doc
                [ unText $ \str -> textAs str plainText
                , unElement $ \tag children ->
                    text "<" <> pprTag tag <> text ">" <>
                    pDocList children <>
                    text "</" <> pprTag tag <> text ">"
                , unSequence $ pDocList
                ]

        pDocList <- share $ \docs ->
            case_ docs
                [ unNil $ text ""
                , unCons $ \head tail ->
                    pDoc head <> pDocList tail
                ]

    pure pDoc

-- Convert to HTML
prettyHTML :: Doc -> PP.Doc ann
prettyHTML = pprMode (flippr $ arg <$> pprHTML)

-- Convert to Markdown
prettyMarkdown :: Doc -> PP.Doc ann
prettyMarkdown = pprMode (flippr $ arg <$> pprMarkdown)

-- Parser for HTML
-- USING THE PARSER SEEM TO RESULT IN INFINITE RECURSION ATM...
-- TODO: FIX THIS
parseHTML :: String -> [Doc]
parseHTML s = case p (stripHtml s) of
    Ok es -> es
    Fail e -> error (show e)
    where
        g :: (G.GrammarD Char g) => g (Err ann Doc)
        g = parsingMode (flippr $ arg <$> pprHTML)
        p = E.parse g

-- Parser for Markdown
parseMarkdown :: String -> [Doc]
parseMarkdown s = case p s of
    Ok es -> es
    Fail e -> error (show e)
    where
        g :: (G.GrammarD Char g) => g (Err ann Doc)
        g = parsingMode (flippr $ arg <$> pprMarkdown)
        p = E.parse g

-- Helper functions
stripHtml :: String -> String
stripHtml s =
    let s' = if "<html>" `isPrefixOf` s then drop 6 s else s
    in if "</html>" `isSuffixOf` s' then take (length s' - 7) s' else s'

-- Test examples
example1 :: Doc
example1 = Text "Hello World"

example2 :: Doc
example2 = Element Bold [Text "Bold text"]

example3 :: Doc
example3 = Element H1 [Text "Main Title"]

example4 :: Doc
example4 = Sequence
    [ Element H1 [Text "Title"]
    , Element P [Text "This is a paragraph with ", Element Bold [Text "bold"], Text " text."]
    , Element H2 [Text "Subtitle"]
    , Element Ul
        [ Element Li [Text "First item"]
        , Element Li [Text "Second item"]
        ]
    ]

-- Test round-trip conversion
checkRoundTrip :: Doc -> String -> IO ()
checkRoundTrip doc name = do
    putStrLn $ "=== Testing " ++ name ++ " ==="

    -- Convert to HTML
    let htmlStr = show (prettyHTML doc)
    putStrLn $ "HTML: " ++ htmlStr

    -- Parse HTML back 
    case parseHTML htmlStr of
        [parsedFromHTML] -> do
            putStrLn $ "HTML round-trip: " ++ show (parsedFromHTML == doc)

            -- Convert to Markdown
            let mdStr = show (prettyMarkdown doc)
            putStrLn $ "Markdown: " ++ mdStr

            -- Parse Markdown back
            case parseMarkdown mdStr of
                [parsedFromMD] -> do
                    putStrLn $ "MD round-trip: " ++ show (parsedFromMD == doc)
                [] -> putStrLn "MD parse failed"
                results -> putStrLn $ "Multiple MD results: " ++ show (length results)

        [] -> putStrLn "HTML parse failed"
        results -> putStrLn $ "Multiple HTML results: " ++ show (length results)

    putStrLn ""

main :: IO ()
main = do
    checkRoundTrip example1 "Simple text"
    checkRoundTrip example2 "Bold text"
    checkRoundTrip example3 "H1 header"
    checkRoundTrip example4 "Complex document"
