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

| **HTML Tag** | **Markdown Syntax**       |
|--------------|---------------------------|
| `<b>`        | `**bold text**`           |
| `<h1>`       | `text\n===`               |
| `<h2>`       | `text\n---`               |
| `<h3>`       | `### text`                |
| `<h4>`       | `#### text`               |
| `<h5>`       | `##### text`              |
| `<p>`        | `text\n`                  |
| `<div>`      | `Concatenation (newlines)`|
| `<li>`       | `- text`                  |
| Sequence     | Concatenation (newlines)  |

## Examples

### HTML to Markdown Conversion

#### Example 1: Simple HTML

**Input (`HtmlExp`):**

    htmlExample1 = Content (Name "helloWorld")

**Output (`HtmlExp`):**
- HTML:
    <html>helloWorld</html>
- Markdown:
    helloWorld

#### Example 2: Nested HTML

**Input (`HtmlExp`):**

    htmlExample2 = Sequence 
      (TagBold (Content (Name "helloWorld")))
      (TagH1 (Content (Name "helloWorld")))
      
**Output (`HtmlExp`):**
- HTML:
<html><b>helloWorld</b><h1>helloWorld</h1></html>
- Markdown:
**helloWorld**

helloWorld
===

### Markdown to HTML Conversion

**Input (`Markdown`):**
**helloWorld**

helloWorld
===
      
**Output (`HtmlExp`):**
Sequence 
  (TagBold (Content (Name "helloWorld")))
  (TagH1 (Content (Name "helloWorld")))


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
    HTML: Add <h6> formatting in htmlPrettyPrinter.
    Markdown: Add ###### formatting in markdownPrettyPrinter.

3. **Update parsers**:
   Add logic to handle the new tag in parseHtml and parseMarkdown.

## About the Project

This project demonstrates the power of invertible transformations using the FliPpr library. By ensuring consistency between input and output formats, it provides a robust solution for parsing and converting structured data to text-based representations.
