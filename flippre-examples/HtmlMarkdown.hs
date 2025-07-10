{- 
Note:
    - `unElement` and `unText` are generated accessors from Template Haskell splices `mkUn`.
    - `unCons` typically destructures lists or trees; `unText` extracts text from a single node.
-}

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

import Text.FliPpr
import qualified Text.FliPpr.Automaton as AM
import qualified Text.FliPpr.Grammar as G
import qualified Text.FliPpr.Grammar.Driver.Earley as E
import qualified Text.FliPpr.QDo as F
-- Parser not included in FliPpr yet. Might be suitable for use in future development
-- import qualified Text.FliPpr.Grammar.Driver.Frost as Fr

import Data.String (fromString)
import Debug.Trace (trace)
import qualified Prettyprinter as PP (Doc)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Text.FliPpr.Grammar as G (pprAsFlat) -- New import check exactly what this does
import Control.Applicative (Alternative(..))

-- AST DATATYPES FOR MARKDOWN

-- | Abstract syntax tree (AST) for a full Markdown document, as a list of block elements.
newtype MarkdownDoc = 
    MarkdownDoc [MarkdownBlock]         -- must be separated by empty lines
    deriving stock (Eq, Show)

-- | Block-level elements in the Markdown AST.
data MarkdownBlock
    = Paragraph [Inline]                -- Paragraph containing inline elements
    | Header Int [Inline]               -- header with level 1-6 and inline content
    | OrderedList [[MarkdownBlock]]     -- Nested ordered lists (outer and inner lists must be non-empty)
    | UnorderedList [[MarkdownBlock]]   -- Nested unordered lists (outer and inner lists must be non-empty)
    deriving stock (Eq, Show)

-- | Inline elements in the Markdown AST.
data Inline = Str String                -- Plain string
    | Strong [Inline]                   -- Bold/strong text
    deriving stock (Eq, Show)

-- Generate "un" accessor functions for above types (ex. unParagraph, unHeader)
$(mkUn ''MarkdownDoc)
$(mkUn ''MarkdownBlock)
$(mkUn ''Inline)

-- AST DATATYPES FOR HTML-LIKE STRUCTURE (used for both HTML and Markdown output)

-- | Abstract syntax tree (AST) for a document with HTML-like structure.
data Doc
    = Text String
    | Element Tag [Doc]
    deriving stock (Eq, Show)

-- | Tags for HTML-like AST elements.
data Tag = Bold | H1 | H2 | H3 | H4 | H5 | P | Div | Li | Ul | Ol
    deriving stock (Eq, Show)

$(mkUn ''Doc)
$(mkUn ''Tag)

-- DFA for plain text (AM.plus instead of AM.star since no empty strings allowed)
plainText :: AM.DFA Char
plainText = AM.plus (AM.unions [ 
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

-- PRETTY-PRINTERS

-- | Pretty-printer for the MarkdownDoc AST.
--   Converts a MarkdownDoc into a pretty-printing expression for Markdown source.
--   Uses FliPpr combinators for recursive structure and formatting.
pprMarkdownDoc :: forall arg exp. (FliPprD arg exp) => FliPprM exp (A arg MarkdownDoc -> E exp D)
pprMarkdownDoc = F.do 
    -- Pretty-print plain text using the DFA for allowed characters.
    let pprText str = textAs str plainText 

    -- Pretty-print headers, supporting both underline and hash styles for levels 1 and 2.
    let pHeaderF level pInlines = 
            case_ level 
                [ is 1 $ pInlines <#> text "\n" <#> text (replicate 20 '=')
                , is 1 $ text "# " <#> pInlines
                , is 2 $ pInlines <#> text "\n" <#> text (replicate 20 '-')
                , is 2 $ text "## " <#> pInlines
                , is 3 $ text "### " <#> pInlines
                , is 4 $ text "#### " <#> pInlines
                , is 5 $ text "##### " <#> pInlines
                , is 6 $ text "###### " <#> pInlines
                ]

    -- Recognize a single space or tab.
    let nb_space :: E exp D = text " " <? text "\t"
    
    -- Recursive combinators for handling empty lines and whitespace.
    rec nb_spaces <- share $ text "" <? (nb_space <#> nb_spaces)
        emptyLine <- share $ nb_spaces <#> text "\n" 
        manyEmptyLines <- share $ text "" <? (emptyLine <#> manyEmptyLines)
        someEmptyLines <- share $ emptyLine <#> manyEmptyLines

    -- Top-level pretty-printer for a MarkdownDoc.
    rec pTop <- share $ \d -> 
            case_ d
                [ unMarkdownDoc $ \blocks -> pBlocks True blocks
                ]

        -- Pretty-print a list of blocks, allowing for optional emptiness.
        pBlocks <- share $ \canBeEmpty bs -> 
            case_ bs 
            [ unNil $ if canBeEmpty then text "" else abort
            , unCons $ \b bs' -> pBlocks' b bs' ] 

        -- Pretty-print a block followed by more blocks, handling empty lines between.
        pBlocks' <- share $ \b bs -> 
            case_ bs 
            [ unNil $ pBlock b <#> manyEmptyLines
            , unCons $ \b' bs' -> 
                pBlock b <#> someEmptyLines <#> pBlocks' b' bs' ]

        -- Pretty-print a single block (paragraph, header, or list).
        pBlock <- share $ \b ->
            case_ b
                [ unParagraph $ \inlines -> pInlines True False inlines
                , unHeader $ \level inlines -> 
                    pHeaderF level (pInlines True False inlines)
                , unOrderedList $ \items -> 
                    pList False True items 
                , unUnorderedList $ \items -> 
                    pList False False items 
                ]

        -- Pretty-print a single list item, with correct prefix for ordered/unordered.
        pListItem <- share $ \isOL bs ->
            let d = if isOL then text "#. " else text "- "
            in d <#> pBlocks False bs 

        -- Pretty-print a list of list items.
        pList <- pure $ \canBeEmpty isOL items ->
            case_ items
                [ unNil $ if canBeEmpty then text "" else abort  
                , unCons $ \item items' -> 
                    pListItem isOL item <#> pList True isOL items'
                ]
        
        -- Pretty-print inline elements (text and strong/bold).
        pInlines <- share $ \canHaveStr canProduceEmpty es ->
            case_ es
                [ unNil $ if canProduceEmpty then text "" else abort 
                , unCons $ \e es' -> 
                    case_ e
                        [ unStr $ \s -> if canHaveStr then pprText s <#> pInlines False True es' else abort 
                        , unStrong $ \inlines -> text "**" <#> pInlines True True inlines <#> text "**" <#> pInlines True True es'
                        ]
                ]
    pure pTop

-- | Pretty-printer for the HTML-like Doc AST, producing Markdown source.
--   Converts a 'Doc' (HTML-like AST) into a pretty-printing expression for Markdown.
--   Uses FliPpr combinators for recursive structure and formatting.
pprMarkdown :: (FliPprD arg exp) => FliPprM exp (A arg Doc -> E exp D)
pprMarkdown = F.do
    -- Pretty-print a tag and its children as Markdown.
    -- pDocList: pretty-printer for a list of Doc nodes (children)
    -- pLiList: pretty-printer for a list of list items (used for lists)
    -- children: the children nodes of the tag
    -- tag: the tag to pretty-print
    let pprTag pDocList pLiList children tag =
            let p = pDocList children in
            case_ tag 
                [ unBold $ text "**" <#> p <#> text "**"
                , unH1 $ p <#> text "\n" <#> text (replicate 20 '=')
                , unH2 $ p <#> text "\n" <#> text (replicate 20 '-')
                , unH3 $ text "### " <#> p
                , unH4 $ text "#### " <#> p
                , unH5 $ text "##### " <#> p
                , unP $ p <#> text "\n\n"
                , unDiv $ p <#> text "\n"
                , unLi $ text "- " <#> p <#> text "\n"
                , unUl $ pLiList True  children
                , unOl $ pLiList False children 
                ]

    -- Pretty-print an element node by dispatching to pprTag.
    -- This is where you could add more logic for tag handling if needed.
    let pprElement tag children pDocList pLiList =
            pprTag pDocList pLiList children tag
        
    -- Pretty-print a list item (li) node, with correct prefix for UL/OL.
    let pprLiElement isInUL tag children pDocList pLiList = 
            case_ tag
                [ unLi $ (if isInUL then text "- " else text "#. ") <#> pDocList children <#> text "\n"
                -- , unUl $ pLiList children
                -- , unOl $ pLiList children
                ]

    -- Pretty-print plain text using the DFA for allowed characters.
    let pprText str = textAs str plainText 

    -- Recursive combinators for handling the Doc AST structure.
    rec 
        -- Pretty-print a single Doc node (either Text or Element).
        pDoc <- share $ \doc ->
            case_ doc
                [ unText $ pprText
                , unElement $ \tag children -> pprElement tag children pDocList pLiListNE -- all tags are handled the same
                ]

        -- Pretty-print a list of Doc nodes, but only Elements (no Text).
        -- Used for cases where Text nodes are not expected (No Head Tag).
        pDocList_NHT <- share $ \docs -> 
            case_ docs 
            [ unNil $ text ""
            , unCons $ \d ds ->
                case_ d  
                [ unElement $ \tag children -> pprElement tag children pDocList pLiListNE <#> pDocList ds ] -- we always unelement after case d
            ]

        -- Pretty-print a full list of Doc nodes, distinguishing Text and Element.
        pDocList <- share $ \docs ->
            case_ docs
                [ unNil $ text ""
                , unCons $ \d ds ->
                    case_ d 
                    [ unText $ \str -> pprText str <#> pDocList_NHT ds 
                    , unElement $ \tag children -> 
                        pprElement tag children pDocList pLiListNE <#> pDocList ds 
                    ]
                ]

        -- Pretty-print a list of list items (li), allowing empty lists.
        pLiList <- share $ \b docs -> 
            case_ docs
                [ unNil $ text ""
                , unCons $ \d ds -> 
                    case_ d 
                    [ unElement $ \tag children -> 
                        pprLiElement b tag children pDocList pLiListNE <#> pLiList b ds 
                    ]
                ]       

        -- Pretty-print a non-empty list of list items (li).
        pLiListNE <- share $ \b docs -> 
            case_ docs
                [ unCons $ \d ds -> 
                    case_ d 
                    [ unElement $ \tag children -> 
                        pprLiElement b tag children pDocList pLiListNE <#> pLiList b ds 
                    ]
                ]       
    pure pDoc

-- | Helper bijection for duplicating a value into a pair (a, a).
--   Used to generate both start and end tags for HTML elements.
dupBij :: Eq a => PartialBij a (a, a) 
dupBij = PartialBij "dup" (\a -> pure (a,a)) (\(a, b) -> if a == b then pure a else Nothing) 

-- | Helper to apply a function to a duplicated input (for tags).
--   Converts x to (x, x) and unpacks for use in h.
dup x h = convertInput dupBij x $ \tags -> unpair tags $ h 

-- | Pretty-printer for the HTML-like Doc AST, producing HTML source.
--   Converts a 'Doc' (HTML-like AST) into a pretty-printing expression for HTML.
--   Uses FliPpr combinators for recursive structure and formatting.
pprHTML :: (FliPprD arg exp) => FliPprM exp (A arg Doc -> E exp D)
pprHTML = F.do
    -- Pretty-print a tag as its HTML name (e.g., "b", "h1", "ul", etc.).
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

    -- Pretty-print an element node as an HTML tag with children.
    -- Uses 'dup' to get both start and end tags.
    let pprElement tag children pDocList =  
         dup tag $ \stag etag ->     -- duplicate tag into start and end                 
            text "<" <#> pprTag stag <#> text ">" <#> -- opening tag
            pDocList children <#>                     -- children content
            text "</" <#> pprTag etag <#> text ">"    -- closing tag

    -- Pretty-print plain text using the DFA for allowed characters.
    let pprText str = textAs str plainText 

    -- Recursive combinators for handling the Doc AST structure.
    rec 
        -- Pretty-print a single Doc node (either Text or Element).
        pDoc <- share $ \doc ->
            case_ doc
                [ unText $ pprText
                , unElement $ \tag children -> pprElement tag children pDocList
                ]

        -- Pretty-print a list of Doc nodes, but only Elements (no Text).
        -- Used for cases where Text nodes are not expected.
        pDocList_NHT <- share $ \docs -> 
            case_ docs 
            [ unNil $ text ""
            , unCons $ \d ds -> -- predefined datastruct used in flippre
                case_ d  
                [ unElement $ \tag children -> pprElement tag children pDocList <#> pDocList ds ] -- we always unelement after case d
            ]

        -- Pretty-print a full list of Doc nodes, distinguishing Text and Element.
        pDocList <- share $ \docs ->
            case_ docs
                [ unNil $ text ""
                , unCons $ \d ds ->
                    case_ d 
                    [ unText $ \str -> pprText str <#> pDocList_NHT ds 
                    , unElement $ \tag children -> 
                        pprElement tag children pDocList <#> pDocList ds 
                    ]
                ]
    pure pDoc


-- CONVERSIONS BETWEEN MarkdownDoc AND Doc

-- Convert MarkdownDoc to Doc
-- THIS NEEDS A MAJOR LOOKOVER
markdownDocToDoc :: MarkdownDoc -> Doc
markdownDocToDoc (MarkdownDoc blocks) = Element Div (map blockToDoc blocks)
    where
        blockToDoc :: MarkdownBlock -> Doc
        blockToDoc (Paragraph inlines) = Element P (map inlineToDoc inlines)
        blockToDoc (Header n inlines) =
            let tag = case n of
                  1 -> H1; 2 -> H2; 3 -> H3; 4 -> H4; 5 -> H5; _ -> P
            in Element tag (map inlineToDoc inlines)
        blockToDoc (OrderedList items) =
            Element Ol (map (\item -> Element Li (map blockToDoc item)) items)
        blockToDoc (UnorderedList items) =
            Element Ul (map (\item -> Element Li (map blockToDoc item)) items)

        inlineToDoc :: Inline -> Doc
        inlineToDoc (Str s) = Text s
        inlineToDoc (Strong xs) = Element Bold (map inlineToDoc xs)

-- Convert Doc to MarkdownDoc
docToMarkdownDoc :: Doc -> MarkdownDoc
docToMarkdownDoc (Element Div blocks) = MarkdownDoc (concatMap docToBlocks blocks)
docToMarkdownDoc d = MarkdownDoc (docToBlocks d)

docToBlocks :: Doc -> [MarkdownBlock]
docToBlocks (Element P inlines) = [Paragraph (concatMap docToInlines inlines)]
docToBlocks (Element tag inlines) = case tag of
    H1 -> [Header 1 (concatMap docToInlines inlines)]
    H2 -> [Header 2 (concatMap docToInlines inlines)]
    H3 -> [Header 3 (concatMap docToInlines inlines)]
    H4 -> [Header 4 (concatMap docToInlines inlines)]
    H5 -> [Header 5 (concatMap docToInlines inlines)]
    Ol -> [OrderedList (map docToBlocksList inlines)]
    Ul -> [UnorderedList (map docToBlocksList inlines)]
    _  -> concatMap docToBlocks inlines
docToBlocks (Text _) = []  -- Text outside block is ignored

docToBlocksList :: Doc -> [MarkdownBlock]
docToBlocksList (Element Li xs) = concatMap docToBlocks xs
docToBlocksList d = docToBlocks d

docToInlines :: Doc -> [Inline]
docToInlines (Text s) = [Str s]
docToInlines (Element Bold xs) = [Strong (concatMap docToInlines xs)]
docToInlines (Element _ xs) = concatMap docToInlines xs



-- | Convert a 'MarkdownDoc' (Markdown AST) to a pretty-printed Markdown document.
prettyMD :: MarkdownDoc -> PP.Doc ann
prettyMD = pprMode (flippr $ arg <$> pprMarkdownDoc)

-- | Convert a 'Doc' (HTML-like AST) to a pretty-printed Markdown document.
prettyMarkdown :: Doc -> PP.Doc ann
prettyMarkdown = pprMode (flippr $ arg <$> pprMarkdown)

-- | Convert a 'Doc' (HTML-like AST) to a pretty-printed HTML document.
prettyHTML :: Doc -> PP.Doc ann
prettyHTML = pprMode (flippr $ arg <$> pprHTML)

-- PARSERS

-- | Parse an HTML string into a list of 'Doc' ASTs.
--   Uses Earley parser and strips <html> tags if present.
--   Includes debug traces for grammar and parse results.
parseHTML :: String -> [Doc]
parseHTML = \s ->
    --trace (show $ G.pprAsFlat $ G.simplify g) $ 
    --trace "Another Trace" $
    case p (stripHtml s) of
        Ok es ->  trace "OK" $ es
        Fail e -> trace "Fail" $ error (show e)
    where
        g :: (G.GrammarD Char g) => g (Err ann Doc)
        g = parsingMode (flippr $ arg <$> pprHTML)
        p = E.parse g

-- | Parse a Markdown string into a list of 'Doc' ASTs (Markdown as HTML-like AST).
--   Includes debug traces for grammar and parse results.
markDownParser :: [Char] -> Err ann [Doc]
markDownParser =
    --trace (show $ G.pprAsFlat $ G.simplify g) $ 
    --trace "Another Trace" $ 
    p
    where
        g :: (G.GrammarD Char g) => g (Err ann Doc)
        g = parsingMode (flippr $ arg <$> pprMarkdown)
        p = E.parse g

-- | Parse a Markdown string into a list of 'MarkdownDoc' ASTs (Markdown AST).
--   Includes debug traces for grammar and parse results.
mdParser :: [Char] -> Err ann [MarkdownDoc]
mdParser =
    --trace (show $ G.pprAsFlat $ G.simplify g) $ 
    --trace "Another Trace" $
    p
    where
        g :: (G.GrammarD Char g) => g (Err ann MarkdownDoc)
        g = parsingMode (flippr $ arg <$> pprMarkdownDoc)
        p = E.parse g

-- | Parse a Markdown string into a list of 'Doc' ASTs.
--   Handles error reporting and tracing.
parseMarkdown :: String -> [Doc]
parseMarkdown s = 
    case markDownParser s of
        Ok es -> es
        Fail e -> error (show e)

-- | Parse a Markdown string into a list of 'MarkdownDoc' ASTs.
--   Handles error reporting and tracing.
parseMarkdownDoc :: String -> [MarkdownDoc]
parseMarkdownDoc s = 
    case mdParser s of
        Ok es -> es
        Fail e -> error (show e)

-- | Remove surrounding <html>...</html> tags from a string, if present.
stripHtml :: String -> String
stripHtml s =
    let s' = if "<html>" `isPrefixOf` s then drop 6 s else s
    in if "</html>" `isSuffixOf` s' then take (length s' - 7) s' else s'

-- TEST EXAMPLES

-- Passed
example1 :: Doc
example1 = Text "Hello World"

-- Passed
example2 :: Doc
example2 = Element Bold [Text "Bold text"]

-- Passed
example3 :: Doc
example3 = Element H1 [Text "Main Title"]

-- Works for html but not markdown as the parser fails for the Ul Li structure
-- FAIL (HTML: OK, MD: Multiple)
-- MD Multiple results: 512255
example4 :: Doc
example4 = Element Div 
    [ Element H1 [Text "Title"]
    , Element P [Text "This is a paragraph with ", Element Bold [Text "bold"], Text " text."]
    , Element H2 [Text "Subtitle"]
    , Element Ul
        [ Element Li [Text "First item"]
        , Element Li [Text "Second item"]
        ]
    ]

-- FAIL (HTML: OK, MD: Multiple)
-- MD Multiple results: 29
example5 :: Doc
example5 = Element P [Text "This is ", Element Bold [Text "very ", Element Bold [Text "bold"]], Text "!"]

-- Passed
example6 :: Doc
example6 = Element Ol
    [ Element Li [Text "First"]
    , Element Li [Text "Second"]
    , Element Li [Text "Third"]
    ]

-- FAIL (HTML: OK, MD: Multiple)
-- MD Multiple results: 5
-- cant identify where ul ends and ol starts
example7 :: Doc
example7 = Element Ul
    [ Element Li [Text "Item 1"]
    , Element Li [Text "Item 2", Element Ul [Element Li [Text "Subitem 2.1"], Element Li [Text "Subitem 2.2"]]]
    , Element Li [Text "Item 3"]
    ]

-- FAIL (HTML: OK, MD: Multiple)
-- MD Multiple results: 43
example8 :: Doc
example8 = Element P [Text "Numbers: 123, punctuation: !?., and more."]

-- Passed
example9 :: Doc
example9 = Element Div []

-- Failed
-- This one gets completely stuck
example10 :: Doc
example10 = Element Div
    [ Element P [Text "Level 1"
        , Element Div [Element P [Text "Level 2"
            , Element Div [Element P [Text "Level 3"]]
        ]]
        ]
    ]

-- Test round-trip conversion
checkRoundTrip :: Doc -> String -> IO ()
checkRoundTrip doc name = do
    let htmlStr = show (prettyHTML doc)
    let htmlParsed = parseHTML htmlStr
    let htmlOk = htmlParsed == [doc]

    let mdStr = show (prettyMarkdown doc)
    let mdParsed = parseMarkdown mdStr
    let mdOk = mdParsed == [doc]

    let htmlStatus = case htmlParsed of
            [parsed] | parsed == doc -> "OK"
            [parsed]                 -> "Mismatch"
            []                       -> "ParseFail"
            xs                       -> "Multiple"
        mdStatus = case mdParsed of
            [parsed] | parsed == doc -> "OK"
            [parsed]                 -> "Mismatch"
            []                       -> "ParseFail"
            xs                       -> "Multiple"

    let pass = htmlStatus == "OK" && mdStatus == "OK"
    putStrLn $ name ++ ": " ++ (if pass then "PASS" else "FAIL")
        ++ " (HTML: " ++ htmlStatus ++ ", MD: " ++ mdStatus ++ ")"

    -- Print only the number of results if there are multiple
    case htmlParsed of
        xs@(_:_:_) -> putStrLn $ "  HTML Multiple results: " ++ show (length xs)
        _ -> pure ()
    case mdParsed of
        --xs@(_:_:_) -> do
        --    putStrLn $ "  MD Multiple results: " ++ show (length xs)
        --    mapM_ (\x -> putStrLn (show x ++ "\n")) xs
        xs@(_:_:_) -> putStrLn $ "  MD Multiple results: " ++ show (length xs)
        _ -> pure ()
    

-- TESTS FOR MarkdownDoc

-- Passed
exampleMD1 :: MarkdownDoc
exampleMD1 = MarkdownDoc [Paragraph [Str "Hello from MarkdownDoc!"]]

-- Passed
exampleMD2 :: MarkdownDoc
exampleMD2 = MarkdownDoc [Paragraph [Str "This is ", Strong [Str "bold"], Str " text."]]

-- Passed
exampleMD3 :: MarkdownDoc
exampleMD3 = MarkdownDoc [Header 1 [Str "Main Title"]]

-- Passed
exampleMD4 :: MarkdownDoc
exampleMD4 = MarkdownDoc
    [ Header 2 [Str "Subtitle"]
    , Paragraph [Str "Some content under a subtitle."]
    ]

-- Passed
exampleMD5 :: MarkdownDoc
exampleMD5 = MarkdownDoc
    [ OrderedList
        [ [Paragraph [Str "First item"]]
        , [Paragraph [Str "Second item"]]
        , [Paragraph [Str "Third item"]]
        ]
    ]

-- Passed
exampleMD6 :: MarkdownDoc
exampleMD6 = MarkdownDoc
    [ UnorderedList
        [ [Paragraph [Str "Item 1"]]
        , [Paragraph [Str "Item 2 ", Strong [Str "with bold"]]]
        , [Paragraph [Str "Item 3"]]
        ]
    ]

-- Failed
-- 2 results
-- MarkdownDoc [OrderedList [[Paragraph [Str "Outer 1"]],[Paragraph [Str "Outer 2"],UnorderedList [[Paragraph [Str "Inner 1"]],[Paragraph [Str "Inner 2"]]]]]],
-- MarkdownDoc [OrderedList [[Paragraph [Str "Outer 1"]],[Paragraph [Str "Outer 2"]]],UnorderedList [[Paragraph [Str "Inner 1"]],[Paragraph [Str "Inner 2"]]]]
exampleMD7 :: MarkdownDoc
exampleMD7 = MarkdownDoc
    [ OrderedList
        [ [Paragraph [Str "Outer 1"]]
        , [Paragraph [Str "Outer 2"]
          , UnorderedList
                [ [Paragraph [Str "Inner 1"]]
                , [Paragraph [Str "Inner 2"]]
                ]
          ]
        ]
    ]

-- Passed
exampleMD8 :: MarkdownDoc
exampleMD8 = MarkdownDoc
    [ Paragraph [Str "First paragraph."]
    , Paragraph [Str "Second paragraph with ", Strong [Str "bold"], Str "."]
    ]

-- Passed
exampleMD9 :: MarkdownDoc
exampleMD9 = MarkdownDoc
    [ Header 3 [Str "List Section"]
    , Paragraph [Str "Below is a list:"]
    , UnorderedList
        [ [Paragraph [Str "Apple"]]
        , [Paragraph [Str "Banana"]]
        , [Paragraph [Str "Cherry"]]
        ]
    ]

-- Passed
exampleMD10 :: MarkdownDoc
exampleMD10 = MarkdownDoc []

checkRoundTripMD :: MarkdownDoc -> String -> IO ()
checkRoundTripMD mdDoc name = do
    let mdStr = show (prettyMD mdDoc)
    let mdParsed = parseMarkdownDoc mdStr
    let status = case mdParsed of
            [parsed] | parsed == mdDoc -> "OK"
            [parsed]                   -> "Mismatch"
            []                         -> "ParseFail"
            xs                         -> "Multiple"
    let pass = status == "OK"
    putStrLn $ name ++ ": " ++ (if pass then "PASS" else "FAIL") ++ " (MD: " ++ status ++ ")"
    case mdParsed of
        xs@(_:_:_) -> putStrLn $ "  MD Multiple results: " ++ show xs ++ show (length xs)
        _ -> pure ()

main :: IO ()
main = do
--    checkRoundTrip example1 "Simple text"
--    checkRoundTrip example2 "Bold text"
--    checkRoundTrip example3 "H1 header"
--    checkRoundTrip example4 "Complex document"
--    checkRoundTrip example5 "Nested bold"
--    checkRoundTrip example6 "Ordered list"
--    checkRoundTrip example7 "Unordered list with nesting"
--    checkRoundTrip example8 "Paragraph with punctuation and numbers"
--    checkRoundTrip example9 "Empty document"
    checkRoundTrip example10 "Deeply nested structure"

mdtest :: IO ()
mdtest = do
    checkRoundTripMD exampleMD1 "Simple MarkdownDoc"
    --checkRoundTripMD exampleMD2 "Paragraph with bold"
    --checkRoundTripMD exampleMD3 "Header 1"
    --checkRoundTripMD exampleMD4 "Header 2 and paragraph"
    --checkRoundTripMD exampleMD5 "Ordered list"
    --checkRoundTripMD exampleMD6 "Unordered list with bold"
    --checkRoundTripMD exampleMD7 "Nested lists"
    --checkRoundTripMD exampleMD8 "Multiple paragraphs"
    --checkRoundTripMD exampleMD9 "Header, paragraph, and list"
    --checkRoundTripMD exampleMD10 "Empty document"

{-
We want the following pipeline: 
Markdown text -> MarkdownDoc
MarkdownDoc -> Markdown text
MarkdownDoc -> Doc           This and the following one are missing
Doc -> MarkdownDoc
Doc -> HTML text
HTML text -> Doc
Doc -> Markdown text
Markdown text -> Doc

We probably need:
markdownDocToDoc :: MarkdownDoc -> Doc
docToMarkdownDoc :: Doc -> MarkdownDoc
-}

pipelineTest :: String -> MarkdownDoc -> IO ()
pipelineTest name mdDoc = do
    putStrLn $ "=== Pipeline test: " ++ name ++ " ==="

    -- 1. MarkdownDoc -> Markdown text
    let mdStr = show (prettyMD mdDoc)

    -- 2. Markdown text -> MarkdownDoc
    let mdParsed = parseMarkdownDoc mdStr
    let mdDoc' = case mdParsed of
            [d] -> d
            _   -> MarkdownDoc []

    -- 3. MarkdownDoc -> Doc
    let doc = markdownDocToDoc mdDoc

    -- 4. Doc -> HTML text
    let htmlStr = show (prettyHTML doc)

    -- 5. HTML text -> Doc
    let docParsed = parseHTML htmlStr
    let doc' = case docParsed of
            [d] -> d
            _   -> Element Div []

    -- 6. Doc -> Markdown text
    let mdStrFromDoc = show (prettyMarkdown doc)

    -- 7. Markdown text -> Doc
    let docParsedFromMD = parseMarkdown mdStrFromDoc
    let docFromMD = case docParsedFromMD of
            [d] -> d
            _   -> Element Div []

    -- 8. Doc -> MarkdownDoc
    let mdDocFromDoc = docToMarkdownDoc doc

    -- 9. Doc (from HTML) -> MarkdownDoc
    let mdDocFromHtml = docToMarkdownDoc doc'

    -- Print results
    putStrLn $ "MD round-trip: " ++ show (mdDoc == mdDoc')
    putStrLn $ "Doc round-trip (HTML): " ++ show (doc == doc')
    putStrLn $ "Doc round-trip (MD): " ++ show (doc == docFromMD)
    putStrLn $ "MD->Doc->MD: " ++ show (mdDoc == mdDocFromDoc)
    putStrLn $ "MD->Doc->HTML->Doc->MD: " ++ show (mdDoc == mdDocFromHtml)
    putStrLn ""

-- Example pipeline test runner
pipelineTests :: IO ()
pipelineTests = do
    pipelineTest "Simple MarkdownDoc" exampleMD1
    pipelineTest "Paragraph with bold" exampleMD2
    pipelineTest "Header 1" exampleMD3
    pipelineTest "Header 2 and paragraph" exampleMD4
    pipelineTest "Ordered list" exampleMD5
    pipelineTest "Unordered list with bold" exampleMD6
    pipelineTest "Nested lists" exampleMD7
    pipelineTest "Multiple paragraphs" exampleMD8
    pipelineTest "Header, paragraph, and list" exampleMD9
    pipelineTest "Empty document" exampleMD10

{-
=== Pipeline test: Simple MarkdownDoc ===
MD round-trip: True
Doc round-trip (HTML): OK
True
Doc round-trip (MD): False
MD->Doc->MD: True
MD->Doc->HTML->Doc->MD: True

=== Pipeline test: Paragraph with bold ===
MD round-trip: True
Doc round-trip (HTML): OK
True
Doc round-trip (MD): False
MD->Doc->MD: True
MD->Doc->HTML->Doc->MD: True

=== Pipeline test: Header 1 ===
MD round-trip: True
Doc round-trip (HTML): OK
True
Doc round-trip (MD): False
MD->Doc->MD: True
MD->Doc->HTML->Doc->MD: True

=== Pipeline test: Header 2 and paragraph ===
MD round-trip: True
Doc round-trip (HTML): OK
True
Doc round-trip (MD): False
MD->Doc->MD: True
MD->Doc->HTML->Doc->MD: True

=== Pipeline test: Ordered list ===
MD round-trip: True
Doc round-trip (HTML): OK
True
Doc round-trip (MD): *** Exception: stack overflow
-}