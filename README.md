# HTML-Markdown Converter

This project implements an **HTML-Markdown Converter** using the FliPpr library. The converter facilitates bidirectional transformations between structured HTML-like expressions (`HtmlExp`) and text representations in both **HTML** and **Markdown** formats.

## Features

- Convert structured `HtmlExp` data to **HTML** or **Markdown** text.
- Parse **HTML** and **Markdown** back into structured `HtmlExp` data.
- Ensure **round-trip correctness** using invertible pretty-printers and parsers.
- Extendable for additional HTML tags and Markdown syntax.
- **Execution timing** for all transformation steps to benchmark performance.

## How It Works

This project uses the FliPpr library to define **invertible transformations**:
- **Pretty-printers**: Format `HtmlExp` into readable HTML or Markdown text.
- **Parsers**: Reconstruct the original `HtmlExp` from text representations.

### Supported HTML and Markdown Elements

| **HTML Tag** | **Markdown Syntax**       | **`HtmlExp` Representation**              |
|--------------|---------------------------|-------------------------------------------|
| `<b>`        | `**bold text**`           | `TagBold HtmlExp`                         |
| `<h1>`       | `text\n===`               | `TagH1 HtmlExp`                           |
| `<h2>`       | `text\n---`               | `TagH2 HtmlExp`                           |
| `<h3>`       | `### text`                | `TagH3 HtmlExp`                           |
| `<h4>`       | `#### text`               | `TagH4 HtmlExp`                           |
| `<h5>`       | `##### text`              | `TagH5 HtmlExp`                           |
| `<p>`        | `text\n`                  | `TagP HtmlExp`                            |
| `<div>`      | Concatenation (newlines)  | `TagDiv HtmlExp`                          |
| `<li>`       | `- text`                  | `TagLi HtmlExp`                           |
| Sequence     | Concatenation (newlines)  | `Sequence HtmlExp HtmlExp`                |
| Plain text   | Plain text (no markup)    | `Content (Name "text")`                   |

## Getting Started

### Prerequisites

Ensure you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed for building and running the project.

### Installation

Clone the repository and navigate to the project directory:

    git clone https://github.com/your-repo/html-markdown-converter.git
    cd flippre

### Build the project using Stack:

    stack build
    
    stack build flippre-examples:exe:html-markdown
    
    stack exec html-markdown

## Code Overview

### Core Data Structures

- **`HtmlExp`**: Represents HTML-like expressions, including tags like `<b>`, `<h1>`, `<p>`, `<div>`, and sequences of elements.
- **`Name`**: Represents text content within tags.

### Core Functions

- **HTML Conversion**:
  - `prettyPrintHtml`: Converts `HtmlExp` to HTML.
  - `parseHtml`: Parses HTML text into `HtmlExp`.
  
- **Markdown Conversion**:
  - `prettyPrintMarkdown`: Converts `HtmlExp` to Markdown.
  - `parseMarkdown`: Parses Markdown text into `HtmlExp`.

### Performance Benchmarking

The `countTime` function benchmarks the execution time for each operation (pretty-printing, parsing, etc.) and ensures computations are fully evaluated using `Control.DeepSeq`.

## How to Extend

To add new tags or Markdown syntax:

1. **Update the `HtmlExp` type**:
   data HtmlExp
     = ...
     | TagH6 HtmlExp -- New tag

2. **Extend the pretty-printers**:
    `HTML: Add <h6> formatting in htmlPrettyPrinter.
    Markdown: Add ###### formatting in markdownPrettyPrinter.`

3. **Update parsers**:
   Add logic to handle the new tag in parseHtml and parseMarkdown.

## Known issues/TODO
1. The sentence length is limited due to recursion that reformats the sequence expression to ensure correct round-trips. This leads to exponentially long execution times when the sentence length increases.

2. TagP and TagDiv are at the moment identical in markdown since the representation of these tags are identical in Markdown. This leads to problems when destinguishing how to convert text from markdown including `<div></div>`.

3. Implement TagUl. Difficulties with using mapping within grammar. Unsure about to which lengths flippre can be used in these situations.

4. The sequence expression should be able to handle a large amount of arguments/expressions but since flipper isn't good at handling mapping I'm unsure about a proper implementation. To this point a lot of issues regarding datastructures and types have arisen when solutions have been tested for. 



## About the Project

This project demonstrates the power of invertible transformations using the FliPpr library. By ensuring consistency between input and output formats, it provides a robust solution for parsing and converting structured data to text-based representations.
