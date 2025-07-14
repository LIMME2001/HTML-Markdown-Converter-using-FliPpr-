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
{-# LANGUAGE DeriveGeneric #-}

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
import Data.Word (Word8)
import Control.Monad (unless)

-- eval
import GHC.Generics (Generic)
import Control.DeepSeq (NFData, rnf)
import System.CPUTime

-- AST DATATYPES FOR MARKDOWN

-- | Abstract syntax tree (AST) for a full Markdown document, as a list of block elements.
newtype MarkdownDoc = 
    MarkdownDoc [MarkdownBlock]         -- must be separated by empty lines
    deriving stock (Eq, Show, Generic)

instance NFData MarkdownDoc

-- | Block-level elements in the Markdown AST.
data MarkdownBlock
    = Paragraph [Inline]                -- Paragraph containing inline elements
    | Header Int [Inline]               -- header with level 1-6 and inline content
    | OrderedList [[MarkdownBlock]]     -- Nested ordered lists (outer and inner lists must be non-empty)
    | UnorderedList [[MarkdownBlock]]   -- Nested unordered lists (outer and inner lists must be non-empty)
    deriving stock (Eq, Show, Generic)

instance NFData MarkdownBlock

-- | Inline elements in the Markdown AST.
data Inline = Str String                -- Plain string
    | Strong [Inline]                   -- Bold/strong text
    deriving stock (Eq, Show, Generic)

instance NFData Inline

-- Generate "un" accessor functions for above types (ex. unParagraph, unHeader)
$(mkUn ''MarkdownDoc)
$(mkUn ''MarkdownBlock)
$(mkUn ''Inline)

-- AST DATATYPES FOR HTML-LIKE STRUCTURE FOR HTML

-- | Abstract syntax tree (AST) for a document with HTML-like structure.
data Doc
    = Text String
    | Element Tag [Doc]
    deriving stock (Eq, Show, Generic)

instance NFData Doc

-- | Tags for HTML-like AST elements.
data Tag = Bold | H1 | H2 | H3 | H4 | H5 | H6 | P | Div | Li | Ul | Ol
    deriving stock (Eq, Show, Generic)

instance NFData Tag

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


-- UNSURE ABOUT THIS BUT ITS CONNECTED TO THE MARKDOWN ONE
data HaveSeenList = HaveSeenOther | HaveSeenOL | HaveSeenUL deriving (Show, Ord, Eq)

instance Enum HaveSeenList where 
    toEnum 0 = HaveSeenOther
    toEnum 1 = HaveSeenOL
    toEnum 2 = HaveSeenUL 
    toEnum _ = error "toEnum: out of range"
    
    fromEnum HaveSeenOther = 0 
    fromEnum HaveSeenOL = 1 
    fromEnum HaveSeenUL = 2 

instance Bounded HaveSeenList where 
    minBound = HaveSeenOther 
    maxBound = HaveSeenUL 

deriving via (G.FromBounded HaveSeenList -> a) instance Arg f a => Arg f (HaveSeenList -> a)

-- PRETTY-PRINTERS

-- | Pretty-printer for the MarkdownDoc AST.
--   Converts a MarkdownDoc into a pretty-printing expression for Markdown source.
--   Uses FliPpr combinators for recursive structure and formatting.
pprMarkdownDoc :: forall arg exp. (FliPprD arg exp) => FliPprM exp (A arg MarkdownDoc -> E exp D)
pprMarkdownDoc = F.do 
    -- Pretty-print plain text using the DFA for allowed characters.
    let pprText str = textAs str plainText 

    -- Pretty-print headers, supporting both underline and hash styles for levels 1 and 2.
    let pHeaderF level pp = 
            case_ level 
                [ is 1 $ pp <#> text "\n" <#> text (replicate 20 '=')
                , is 1 $ text "# " <#> pp
                , is 2 $ pp <#> text "\n" <#> text (replicate 20 '-')
                , is 2 $ text "## " <#> pp
                , is 3 $ text "### " <#> pp
                , is 4 $ text "#### " <#> pp
                , is 5 $ text "##### " <#> pp
                , is 6 $ text "###### " <#> pp
                ]

    -- Recognize a single space or tab.
    let nb_space :: E exp D = text " " <? text "\t"
    
    -- Recursive combinators for handling empty lines and whitespace.
    rec nb_spaces <- share $ text "" <? (nb_space <#> nb_spaces)
        emptyLine <- share $ nb_spaces <#> text "\n" 
        manyEmptyLines <- share $ text "" <? (emptyLine <#> manyEmptyLines)
        someEmptyLines <- share $ emptyLine <#> manyEmptyLines

    let indent off = foldr (<#>) (text "") $ replicate (fromIntegral off) (text " ") 
    let indentUnless off b d = if not b then indent off <#> d else d  
    
    let cutOff :: Word8 -> E exp D -> E exp D 
        cutOff off d = if off > 16 then abort else d  

    let withIncIndent f = f 0 <? f 1 <? f 2  
    -- Top-level pretty-printer for a MarkdownDoc.
    rec pTop <- share $ \d -> 
            case_ d
                [ unMarkdownDoc $ \blocks -> manyEmptyLines <#> pBlocks 0 False True HaveSeenOther blocks <#> manyEmptyLines
                ]

        -- Pretty-print a list of blocks, allowing for optional emptiness.
        pBlocks <- share $ \off noIndent canBeEmpty seen bs -> cutOff off $ 
            case_ bs 
            [ unNil $ if canBeEmpty then text "" else abort
            , unCons $ \b bs' -> pBlocks' off noIndent seen b bs' ] 

        -- Pretty-print a block followed by more blocks, handling empty lines between.
        pBlocks' <- share $ \off noIndent seen b bs -> cutOff off $ 
            let pBlock rest = case_ b 
                    [ unParagraph $ \inlines -> 
                        (indentUnless off noIndent $ pInlines True False inlines) <#> rest HaveSeenOther
                    , unHeader $ \level inlines -> 
                        (indentUnless off noIndent $ pHeaderF level (pInlines True False inlines)) <#> rest HaveSeenOther
                    , unOrderedList $ \items -> 
                        case seen of 
                            HaveSeenOL -> abort 
                            _          -> pList off noIndent False True items <#> rest HaveSeenOL
                    , unUnorderedList $ \items -> 
                        case seen of 
                            HaveSeenUL -> abort 
                            _ -> pList off noIndent False False items <#> rest HaveSeenUL 
                    ]
            in case_ bs 
            [ unNil $ pBlock $ \_ -> text "" 
            , unCons $ \b' bs' -> 
                let rest s = someEmptyLines <#> pBlocks' off False s b' bs' 
                in pBlock rest 
  
                ]

        -- -- Pretty-print a single block (paragraph, header, or list).
        -- pBlock <- share $ \off noIndent b -> cutOff off $
        --     case_ b
        --         [ unParagraph $ \inlines -> indentUnless off noIndent $ pInlines True False inlines
        --         , unHeader $ \level inlines -> 
        --             indentUnless off noIndent $ pHeaderF level (pInlines True False inlines)
        --         , unOrderedList $ \items -> 
        --             pList off noIndent False True items 
        --         , unUnorderedList $ \items -> 
        --             pList off noIndent False False items 
        --         ]

        -- Pretty-print a single list item, with correct prefix for ordered/unordered.
        pListItem <- share $ \(off :: Word8) (noIndent :: Bool) isOL bs -> cutOff off $ 
            let dstr = if isOL then "#. " else "- " 
                d = text dstr 
            in withIncIndent $ \inc -> 
                indentUnless (off + inc) noIndent $ 
                 d <#> pBlocks (off + inc + fromIntegral (length dstr)) True False HaveSeenOther bs 

        -- Pretty-print a list of list items.
        pList <- pure $ \off noIndent canBeEmpty isOL items -> cutOff off $
            case_ items
                [ unNil $ if canBeEmpty then text "" else abort  
                , unCons $ \item items' -> pList' off noIndent isOL item items'
                ]
        
        pList' <- share $ \(off :: Word8) (noIndent :: Bool) isOL b bs -> cutOff off $ 
            case_ bs 
            [ unNil  $ pListItem off noIndent isOL b 
            , unCons $ \b' bs' -> pListItem off noIndent isOL b <#> someEmptyLines <#> pList' off False isOL b' bs' ]
        -- Pretty-print inline elements (text and strong/bold).
        pInlines <- share $ \(canHaveStr :: Bool) canProduceEmpty es ->  
            case_ es 
                [ unNil $ if canProduceEmpty then text "" else abort 
                , unCons $ \e es' -> 
                    case_ e
                        [ unStr $ \s -> if canHaveStr then pprText s <#> pInlines False True es' else abort 
                        , unStrong $ \inlines -> text "**" <#> pInlines True True inlines <#> text "**" <#> pInlines True True es'
                        ]
                ]
    pure pTop


-- OLD CODE NOT USED ANYMORE

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
            , unH6 $ text "h6"
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


-- TEST EXAMPLES HTML

-- Passed
example1 :: Doc
example1 = Text "Hello World"

-- Passed
example2 :: Doc
example2 = Element Bold [Text "Bold text"]

-- Passed
example3 :: Doc
example3 = Element H1 [Text "Main Title"]

-- Passed
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

-- Passed
example5 :: Doc
example5 = Element P [Text "This is ", Element Bold [Text "very ", Element Bold [Text "bold"]], Text "!"]

-- Passed
example6 :: Doc
example6 = Element Ol
    [ Element Li [Text "First"]
    , Element Li [Text "Second"]
    , Element Li [Text "Third"]
    ]

-- Passed
example7 :: Doc
example7 = Element Ul
    [ Element Li [Text "Item 1"]
    , Element Li [Text "Item 2", Element Ul [Element Li [Text "Subitem 2.1"], Element Li [Text "Subitem 2.2"]]]
    , Element Li [Text "Item 3"]
    ]

-- Passed
example8 :: Doc
example8 = Element P [Text "Numbers: 123, punctuation: !?., and more."]

-- Passed
example9 :: Doc
example9 = Element Div []

-- Passed
example10 :: Doc
example10 = Element Div
    [ Element P [Text "Level 1"
        , Element Div [Element P [Text "Level 2"
            , Element Div [Element P [Text "Level 3"]]
        ]]
        ]
    ]

checkRoundTripHTML :: Doc -> String -> IO ()
checkRoundTripHTML doc name = do
    let htmlStr = show (prettyHTML doc)
    let htmlParsed = parseHTML htmlStr
    let status = case htmlParsed of
            [parsed] | parsed == doc -> "OK"
            [parsed]                 -> "Mismatch"
            []                       -> "ParseFail"
            xs                       -> "Multiple"
    let pass = status == "OK"
    putStrLn $ name ++ ": " ++ (if pass then "PASS" else "FAIL") ++ " (HTML: " ++ status ++ ")"
    case htmlParsed of
        xs@(_:_:_) -> putStrLn $ "  HTML Multiple results: " ++ show (length xs)
        _ -> pure ()

htmltest :: IO ()
htmltest = do
    checkRoundTripHTML example1 "Simple text"
    checkRoundTripHTML example2 "Bold text"
    checkRoundTripHTML example3 "H1 header"
    checkRoundTripHTML example4 "Complex document"
    checkRoundTripHTML example5 "Nested bold"
    checkRoundTripHTML example6 "Ordered list"
    checkRoundTripHTML example7 "Unordered list with nesting"
    checkRoundTripHTML example8 "Paragraph with punctuation and numbers"
    checkRoundTripHTML example9 "Empty document"
    checkRoundTripHTML example10 "Deeply nested structure"


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

-- Passed
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

mdtest :: IO ()
mdtest = do
    checkRoundTripMD exampleMD1 "Simple MarkdownDoc"
    checkRoundTripMD exampleMD2 "Paragraph with bold"
    checkRoundTripMD exampleMD3 "Header 1"
    checkRoundTripMD exampleMD4 "Header 2 and paragraph"
    checkRoundTripMD exampleMD5 "Ordered list"
    checkRoundTripMD exampleMD6 "Unordered list with bold"
    checkRoundTripMD exampleMD7 "Nested lists"
    checkRoundTripMD exampleMD8 "Multiple paragraphs"
    checkRoundTripMD exampleMD9 "Header, paragraph, and list"
    checkRoundTripMD exampleMD10 "Empty document"


-- CONVERSIONS BETWEEN MarkdownDoc AND Doc

-- Convert MarkdownDoc to Doc (HTML-like AST)
markdownDocToDoc :: MarkdownDoc -> Doc
markdownDocToDoc (MarkdownDoc []) = Element Div []
markdownDocToDoc (MarkdownDoc [block]) = blockToDoc block  -- Single block doesn't need Div wrapper, needed for correct roundtrips
markdownDocToDoc (MarkdownDoc blocks) = Element Div (map blockToDoc blocks)

blockToDoc :: MarkdownBlock -> Doc
blockToDoc (Paragraph inlines) = Element P (inlinesToDocs inlines)
blockToDoc (Header n inlines) =
  let tag = case n of
        1 -> H1; 2 -> H2; 3 -> H3; 4 -> H4; 5 -> H5; 6 -> H6; _ -> error $ "Invalid header level: " ++ show n ++ ". Valid levels are 1-6."
  in Element tag (inlinesToDocs inlines)
blockToDoc (OrderedList items) = Element Ol (map (\item -> Element Li (map blockToDoc item)) items)
blockToDoc (UnorderedList items) = Element Ul (map (\item -> Element Li (map blockToDoc item)) items)

inlinesToDocs :: [Inline] -> [Doc]
inlinesToDocs = concatMap inlineToDocs

inlineToDocs :: Inline -> [Doc]
inlineToDocs (Str s) = [Text s]
inlineToDocs (Strong xs) = [Element Bold (inlinesToDocs xs)]

-- Convert Doc to MarkdownDoc
docToMarkdownDoc :: Doc -> MarkdownDoc
docToMarkdownDoc (Element Div blocks) = MarkdownDoc (map docToBlock blocks)
docToMarkdownDoc (Element Div []) = MarkdownDoc []
docToMarkdownDoc doc = MarkdownDoc [docToBlock doc]

docToBlock :: Doc -> MarkdownBlock
docToBlock (Element P inlines) = Paragraph (docsToInlines inlines)
docToBlock (Element H1 inlines) = Header 1 (docsToInlines inlines)
docToBlock (Element H2 inlines) = Header 2 (docsToInlines inlines)
docToBlock (Element H3 inlines) = Header 3 (docsToInlines inlines)
docToBlock (Element H4 inlines) = Header 4 (docsToInlines inlines)
docToBlock (Element H5 inlines) = Header 5 (docsToInlines inlines)
docToBlock (Element H6 inlines) = Header 6 (docsToInlines inlines)
docToBlock (Element Ol items) = OrderedList (map liToBlocks items)
docToBlock (Element Ul items) = UnorderedList (map liToBlocks items)
docToBlock (Element Bold inlines) = Paragraph [Strong (docsToInlines inlines)]
docToBlock (Element Div blocks) = Paragraph (docsToInlines blocks)  -- Flatten div to paragraph (hopefully ok)
{-
This gives:
  Doc -> MarkdownDoc: *** Exception: Cannot convert multi-element div to single block
CallStack (from HasCallStack):
  error, called at C:\Users\linus\Documents\my VS code\flippr\flippre\flippre-examples\HtmlMarkdown.hs:742:10 in main:Main
  
docToBlock (Element Div blocks) = 
  case blocks of
    [single] -> docToBlock single  -- Only flatten single elements
    _ -> error "Cannot convert multi-element div to single block"
-}
docToBlock (Text s) = Paragraph [Str s]
docToBlock _ = Paragraph []

liToBlocks :: Doc -> [MarkdownBlock]
liToBlocks (Element Li blocks) = map docToBlock blocks
liToBlocks _ = []

docsToInlines :: [Doc] -> [Inline]
docsToInlines = concatMap docToInlines

docToInlines :: Doc -> [Inline]
docToInlines (Text s) = [Str s]
docToInlines (Element Bold xs) = [Strong (docsToInlines xs)]
docToInlines (Element P xs) = docsToInlines xs  -- Flatten paragraph content
docToInlines _ = []  -- Ignore other elements at inline level

-- CONVERSION TESTS

-- Test cases for MarkdownDoc to Doc conversion
conversionTestMD1 :: MarkdownDoc
conversionTestMD1 = MarkdownDoc [Paragraph [Str "Simple text"]]

conversionTestMD2 :: MarkdownDoc
conversionTestMD2 = MarkdownDoc [Paragraph [Str "Text with ", Strong [Str "bold"], Str " content"]]

conversionTestMD3 :: MarkdownDoc
conversionTestMD3 = MarkdownDoc [Header 1 [Str "Main Title"]]

conversionTestMD4 :: MarkdownDoc
conversionTestMD4 = MarkdownDoc [Header 2 [Str "Subtitle with ", Strong [Str "bold"]]]

conversionTestMD5 :: MarkdownDoc
conversionTestMD5 = MarkdownDoc 
    [ OrderedList
        [ [Paragraph [Str "First item"]]
        , [Paragraph [Str "Second item"]]
        ]
    ]

conversionTestMD6 :: MarkdownDoc
conversionTestMD6 = MarkdownDoc 
    [ UnorderedList
        [ [Paragraph [Str "Item A"]]
        , [Paragraph [Str "Item B with ", Strong [Str "bold"]]]
        ]
    ]

conversionTestMD7 :: MarkdownDoc
conversionTestMD7 = MarkdownDoc 
    [ Header 1 [Str "Title"]
    , Paragraph [Str "Some text"]
    , OrderedList
        [ [Paragraph [Str "List item 1"]]
        , [Paragraph [Str "List item 2"]]
        ]
    ]

conversionTestMD8 :: MarkdownDoc
conversionTestMD8 = MarkdownDoc 
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

conversionTestMD9 :: MarkdownDoc
conversionTestMD9 = MarkdownDoc []

conversionTestMD10 :: MarkdownDoc
conversionTestMD10 = MarkdownDoc [Header 6 [Str "Level 6 header"]]

-- Test cases for Doc to MarkdownDoc conversion
conversionTestDoc1 :: Doc
conversionTestDoc1 = Element P [Text "Simple paragraph"]

conversionTestDoc2 :: Doc
conversionTestDoc2 = Element P [Text "Text with ", Element Bold [Text "bold"], Text " content"]

conversionTestDoc3 :: Doc
conversionTestDoc3 = Element H1 [Text "Main Title"]

conversionTestDoc4 :: Doc
conversionTestDoc4 = Element Div
    [ Element H1 [Text "Title"]
    , Element P [Text "Some text"]
    , Element Ul
        [ Element Li [Element P [Text "Item 1"]]
        , Element Li [Element P [Text "Item 2"]]
        ]
    ]

conversionTestDoc5 :: Doc
conversionTestDoc5 = Element Ol
    [ Element Li [Element P [Text "First"]]
    , Element Li [Element P [Text "Second"]]
    ]

conversionTestDoc6 :: Doc
conversionTestDoc6 = Element Div []

conversionTestDoc7 :: Doc
conversionTestDoc7 = Text "Just text"

-- Function to test MarkdownDoc -> Doc -> MarkdownDoc round-trip
testMDRoundTrip :: MarkdownDoc -> String -> IO ()
testMDRoundTrip original name = do
    let converted = docToMarkdownDoc (markdownDocToDoc original)
    let success = converted == original
    putStrLn $ name ++ " (MD->Doc->MD): " ++ (if success then "PASS" else "FAIL")
    unless success $ do
        putStrLn $ "  Original: " ++ show original
        putStrLn $ "  Result:   " ++ show converted

-- Function to test Doc -> MarkdownDoc -> Doc round-trip
testDocRoundTrip :: Doc -> String -> IO ()
testDocRoundTrip original name = do
    let converted = markdownDocToDoc (docToMarkdownDoc original)
    let success = converted == original
    putStrLn $ name ++ " (Doc->MD->Doc): " ++ (if success then "PASS" else "FAIL")
    unless success $ do
        putStrLn $ "  Original: " ++ show original
        putStrLn $ "  Result:   " ++ show converted

-- Function to test individual conversions
testConversion :: MarkdownDoc -> Doc -> String -> IO ()
testConversion md doc name = do
    let mdToDoc = markdownDocToDoc md
    let docToMd = docToMarkdownDoc doc
    let mdSuccess = mdToDoc == doc
    let docSuccess = docToMd == md
    putStrLn $ name ++ " (MD->Doc): " ++ (if mdSuccess then "PASS" else "FAIL")
    putStrLn $ name ++ " (Doc->MD): " ++ (if docSuccess then "PASS" else "FAIL")
    unless mdSuccess $ do
        putStrLn $ "  MD->Doc Expected: " ++ show doc
        putStrLn $ "  MD->Doc Actual:   " ++ show mdToDoc
    unless docSuccess $ do
        putStrLn $ "  Doc->MD Expected: " ++ show md
        putStrLn $ "  Doc->MD Actual:   " ++ show docToMd

-- Function to test if conversion preserves semantic meaning (even if structure differs)
testSemanticEquivalence :: MarkdownDoc -> String -> IO ()
testSemanticEquivalence md name = do
    let doc = markdownDocToDoc md
    let backToMd = docToMarkdownDoc doc
    let mdStr = show (prettyMD md)
    let backStr = show (prettyMD backToMd)
    let success = mdStr == backStr
    putStrLn $ name ++ " (semantic): " ++ (if success then "PASS" else "FAIL")
    unless success $ do
        putStrLn $ "  Original MD: " ++ mdStr
        putStrLn $ "  Round-trip:  " ++ backStr

-- Run all conversion tests
conversionTests :: IO ()
conversionTests = do
    putStrLn "=== Conversion Tests ==="
    
    putStrLn "\n--- Round-trip Tests (MD->Doc->MD) ---"
    testMDRoundTrip conversionTestMD1 "Simple text"
    testMDRoundTrip conversionTestMD2 "Text with bold"
    testMDRoundTrip conversionTestMD3 "Header 1"
    testMDRoundTrip conversionTestMD4 "Header with bold"
    testMDRoundTrip conversionTestMD5 "Ordered list"
    testMDRoundTrip conversionTestMD6 "Unordered list with bold"
    testMDRoundTrip conversionTestMD7 "Complex document"
    testMDRoundTrip conversionTestMD8 "Nested lists"
    
    putStrLn "\n--- Round-trip Tests (Doc->MD->Doc) ---"
    testDocRoundTrip conversionTestDoc1 "Simple paragraph"
    testDocRoundTrip conversionTestDoc2 "Paragraph with bold"
    testDocRoundTrip conversionTestDoc3 "H1 header"
    testDocRoundTrip conversionTestDoc4 "Complex document"
    testDocRoundTrip conversionTestDoc5 "Ordered list"
    testDocRoundTrip conversionTestDoc6 "Empty div"
    testDocRoundTrip conversionTestDoc7 "Just text"
    
    putStrLn "\n--- Additional MD Round-trip Tests ---"
    testMDRoundTrip conversionTestMD9 "Empty document"
    testMDRoundTrip conversionTestMD10 "Header level 6"
    
    putStrLn "\n--- Semantic Equivalence Tests ---"
    testSemanticEquivalence conversionTestMD1 "Simple text"
    testSemanticEquivalence conversionTestMD2 "Text with bold"
    testSemanticEquivalence conversionTestMD3 "Header 1"
    testSemanticEquivalence conversionTestMD5 "Ordered list"
    testSemanticEquivalence conversionTestMD6 "Unordered list with bold"

-- Helper function to inspect conversion results
inspectConversion :: MarkdownDoc -> IO ()
inspectConversion md = do
    putStrLn $ "Original MD: " ++ show md
    let doc = markdownDocToDoc md
    putStrLn $ "Converted to Doc: " ++ show doc
    let backToMd = docToMarkdownDoc doc
    putStrLn $ "Back to MD: " ++ show backToMd
    putStrLn $ "Round-trip successful: " ++ show (md == backToMd)
    putStrLn ""


-- =================================================================
-- ============= COMPREHENSIVE TEST SUITE ==========================
-- =================================================================
-- This section contains a full test suite for the dual-way
-- Markdown and HTML converter.

-- | A data type to hold a single test case, containing both its
--   Markdown and HTML string representations, and its AST forms.
data TestCase = TestCase
  { name         :: String
  , markdownStr  :: String
  , htmlStr      :: String
  , markdownAST  :: MarkdownDoc
  , docAST       :: Doc
  }

-- | A list of test cases to be used in the test suite.
testCases :: [TestCase]
testCases =
  [ TestCase
      "Simple Paragraph"
      "Just a simple paragraph."
      "<p>Just a simple paragraph.</p>"
      (MarkdownDoc [Paragraph [Str "Just a simple paragraph."]])
      (Element P [Text "Just a simple paragraph."])
  , TestCase
      "Header Level 1"
      "Title\n===================="
      "<h1>Title</h1>"
      (MarkdownDoc [Header 1 [Str "Title"]])
      (Element H1 [Text "Title"])
  , TestCase
      "Bold Text"
      "This has **bold** text."
      "<p>This has <b>bold</b> text.</p>"
      (MarkdownDoc [Paragraph [Str "This has ", Strong [Str "bold"], Str " text."]])
      (Element P [Text "This has ", Element Bold [Text "bold"], Text " text."])
  , TestCase
      "Unordered List"
      "- Item 1\n- Item 2"
      "<ul><li><p>Item 1</p></li><li><p>Item 2</p></li></ul>"
      (MarkdownDoc [UnorderedList [[Paragraph [Str "Item 1"]], [Paragraph [Str "Item 2"]]]])
      (Element Ul [Element Li [Element P [Text "Item 1"]], Element Li [Element P [Text "Item 2"]]])
  , TestCase
      "Ordered List"
      "#. First\n#. Second"
      "<ol><li><p>First</p></li><li><p>Second</p></li></ol>"
      (MarkdownDoc [OrderedList [[Paragraph [Str "First"]], [Paragraph [Str "Second"]]]])
      (Element Ol [Element Li [Element P [Text "First"]], Element Li [Element P [Text "Second"]]])
  , TestCase
      "Complex Document"
      "A Title\n====================\nSome text here.\n- A list item\n- Another one"
      "<div><h1>A Title</h1><p>Some text here.</p><ul><li><p>A list item</p></li><li><p>Another one</p></li></ul></div>"
      (MarkdownDoc [Header 1 [Str "A Title"], Paragraph [Str "Some text here."], UnorderedList [[Paragraph [Str "A list item"]], [Paragraph [Str "Another one"]]]])
      (Element Div [Element H1 [Text "A Title"], Element P [Text "Some text here."], Element Ul [Element Li [Element P [Text "A list item"]], Element Li [Element P [Text "Another one"]]]])
  , TestCase
      "Nested Lists"
      "- Outer 1\n#. Inner A\n#. Inner B\n- Outer 2"
      "<ul><li><div><p>Outer 1</p><ol><li><p>Inner A</p></li><li><p>Inner B</p></li></ol></div></li><li><p>Outer 2</p></li></ul>"
      (MarkdownDoc [UnorderedList [[Paragraph [Str "Outer 1"], OrderedList [[Paragraph [Str "Inner A"]], [Paragraph [Str "Inner B"]]]], [Paragraph [Str "Outer 2"]]]])
      (Element Ul [Element Li [Element Div [Element P [Text "Outer 1"], Element Ol [Element Li [Element P [Text "Inner A"]], Element Li [Element P [Text "Inner B"]]]]], Element Li [Element P [Text "Outer 2"]]])
  ]

-- | Main entry point for running all tests.
runTests :: IO ()
runTests = do
    putStrLn "Running Comprehensive Test Suite..."
    mapM_ runSingleTestCase testCases
    putStrLn "\nAll tests complete."

-- | Runs all checks for a single TestCase.
runSingleTestCase :: TestCase -> IO ()
runSingleTestCase tc = do
    putStrLn $ "\n--- Testing: " ++ name tc ++ " ---"
    -- Test Parsing
    test "Markdown Parsing" (head (parseMarkdownDoc (markdownStr tc))) (markdownAST tc)
    test "HTML Parsing" (head (parseHTML (htmlStr tc))) (docAST tc)

    -- Test Pretty Printing
    test "Markdown Pretty Printing" (show . prettyMD . markdownAST $ tc) (markdownStr tc)
    test "HTML Pretty Printing" (show . prettyHTML . docAST $ tc) (htmlStr tc)

    -- Test AST Conversions
    test "MarkdownDoc -> Doc" (markdownDocToDoc (markdownAST tc)) (docAST tc)
    test "Doc -> MarkdownDoc" (docToMarkdownDoc (docAST tc)) (markdownAST tc)

    -- Test Round Trips
    testPipeline "MD -> AST -> MD" (show . prettyMD . head . parseMarkdownDoc) (markdownStr tc)
    testPipeline "HTML -> AST -> HTML" (show . prettyHTML . head . parseHTML) (htmlStr tc)
    testPipeline "MD AST -> Doc -> MD AST" (docToMarkdownDoc . markdownDocToDoc) (markdownAST tc)
    testPipeline "Doc AST -> MD -> Doc AST" (markdownDocToDoc . docToMarkdownDoc) (docAST tc)

-- | A generic test function to compare an actual result with an expected result.
test :: (Eq a, Show a) => String -> a -> a -> IO ()
test testName actual expected = do
    let pass = actual == expected
    putStrLn $ "  " ++ testName ++ ": " ++ if pass then "PASS" else "FAIL"
    unless pass $ do
        putStrLn $ "    Expected: " ++ show expected
        putStrLn $ "    Actual:   " ++ show actual

-- | A specialized test function for round-trip conversions.
testPipeline :: (Eq a, Show a) => String -> (a -> a) -> a -> IO ()
testPipeline testName fn initial = do
    let result = fn initial
    let pass = result == initial
    putStrLn $ "  " ++ testName ++ ": " ++ if pass then "PASS" else "FAIL"
    unless pass $ do
        putStrLn $ "    Initial: " ++ show initial
        putStrLn $ "    Result:  " ++ show result

-- To run these tests, you can replace the existing `main` function with:
-- main :: IO ()
-- main = runTests


countTime :: NFData a => String -> IO a -> IO a
countTime label computation = do
    start <- getCPUTime
    result <- computation
    rnf result `seq` return ()
    end <- getCPUTime
    let durationMs = fromIntegral (end - start) / (10 ^ 9)
    putStrLn $ label ++ " - Elapsed: " ++ show durationMs ++ " ms"
    return result

benchmarkMarkdownRoundTrip :: MarkdownDoc -> IO ()
benchmarkMarkdownRoundTrip md = do
  _ <- countTime "Markdown <-> HTML <-> Markdown" $
    return $ docToMarkdownDoc (markdownDocToDoc md)
  return ()

benchmarkHtmlRoundTrip :: Doc -> IO ()
benchmarkHtmlRoundTrip html = do
  _ <- countTime "HTML <-> Markdown <-> HTML" $
    return $ markdownDocToDoc (docToMarkdownDoc html)
  return ()

runBenchmarks :: IO ()
runBenchmarks = do
  putStrLn $ replicate 40 '='
  putStrLn "Running Round-Trip Performance Benchmarks"
  putStrLn $ replicate 40 '='

  benchmarkMarkdownRoundTrip conversionTestMD1
  benchmarkMarkdownRoundTrip conversionTestMD7
  benchmarkHtmlRoundTrip conversionTestDoc1
  benchmarkHtmlRoundTrip conversionTestDoc5
