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
        --  dup tag $ \prefixTag suffixTag ->     -- duplicate tag into prefix and suffix                   
        --     pprTagPrefix prefixTag <>         -- markdown prefix
        --     pDocList children <>              -- content
        --     pprTagSuffix suffixTag             -- markdown suffix
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
    trace (show $ G.pprAsFlat $ G.simplify g) $ 
    trace "Another Trace" $
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
    trace (show $ G.pprAsFlat $ G.simplify g) $ 
    trace "Another Trace" p
    where
        g :: (G.GrammarD Char g) => g (Err ann Doc)
        g = parsingMode (flippr $ arg <$> pprMarkdown)
        p = E.parse g

-- | Parse a Markdown string into a list of 'MarkdownDoc' ASTs (Markdown AST).
--   Includes debug traces for grammar and parse results.
mdParser :: [Char] -> Err ann [MarkdownDoc]
mdParser =
    trace (show $ G.pprAsFlat $ G.simplify g) $ 
    trace "Another Trace" p
    where
        g :: (G.GrammarD Char g) => g (Err ann MarkdownDoc)
        g = parsingMode (flippr $ arg <$> pprMarkdownDoc)
        p = E.parse g

-- | Parse a Markdown string into a list of 'Doc' ASTs.
--   Handles error reporting and tracing.
parseMarkdown :: String -> [Doc]
parseMarkdown s = 
    case markDownParser s of
        Ok es ->  trace "OK" $ es
        Fail e -> trace "Fail" $ error (show e)

-- | Parse a Markdown string into a list of 'MarkdownDoc' ASTs.
--   Handles error reporting and tracing.
parseMarkdownDoc :: String -> [MarkdownDoc]
parseMarkdownDoc s = 
    case mdParser s of
        Ok es ->  trace "OK" $ es
        Fail e -> trace "Fail" $ error (show e)

-- | Remove surrounding <html>...</html> tags from a string, if present.
stripHtml :: String -> String
stripHtml s =
    let s' = if "<html>" `isPrefixOf` s then drop 6 s else s
    in if "</html>" `isSuffixOf` s' then take (length s' - 7) s' else s'

-- TEST EXAMPLES

example1 :: Doc
example1 = Text "Hello World"

example2 :: Doc
example2 = Element Bold [Text "Bold text"]

example3 :: Doc
example3 = Element H1 [Text "Main Title"]

-- Works for html but not markdown as the parser fails for the Ul Li structure
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
