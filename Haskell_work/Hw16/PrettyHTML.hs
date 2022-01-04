-- Lewis Arnsten
-- Exercise 16.1

{- A module for producing "pretty" HTML. -}

module Pretty where

import Control.Monad.State

type Document = State (Int, String)

{- Append a String to the HTML document. -}

string :: String -> Document ()
string t = modify (\(n, c) -> (n, c ++ t)) 

{- Append a newline, indenting the subsequent text. -}

newline :: Document ()
newline = modify (\(n, c) -> (n, c ++ "\n" ++ (concat $ take n $ repeat "    "))) 

{- Render a Document as Text -}

render :: Document a -> String
render doc = snd (execState doc (0,""))

{- Increase the indentation level. -}

indent :: Document ()
indent = modify (\(n, c) -> (n + 1, c)) 

{- Decrease the indentation level. -}

exdent :: Document ()
exdent = modify (\(n, c) -> (n - 1, c)) 

{- An HTML tag which indents its content -}

type HTML = Document ()

tag :: String -> HTML -> HTML
tag t html = do
    newline
    inlineTag t $ do
        indent
        html
        exdent
        newline

{- An HTML tag which does not indent its content -}

inlineTag :: String -> HTML -> HTML
inlineTag t html = do
    string $ "<" ++ t ++ ">"
    void html
    string $ "</" ++ t ++ ">"

{- An HTML tag which presents its content on a new line -}

onelineTag :: String -> HTML -> HTML
onelineTag t html = newline >> inlineTag t html

{- Provide the functionality of the standard HTML tags -}

html, head, title, body, p, i, b, h1, h2, h3, h4, ol, ul, li, table, tr, th, td
    :: HTML -> HTML
html  = tag "html"
head  = tag "head"
title = onelineTag "title"
body  = tag "body"
p     = onelineTag "p"
i     = inlineTag "i"
b     = inlineTag "b"
h1    = onelineTag "h1"
h2    = onelineTag "h2"
h3    = onelineTag "h3"
h4    = onelineTag "h4"
ol    = tag "ol"
ul    = tag "ul"
li    = onelineTag  "li"
table = tag "table"
tr    = tag "tr"
th    = tag "th"
td    = tag "td"

testDoc :: HTML
testDoc = html $ do 
		    body $ do
			    p $ do
				    string "Testing string"

-- render testDoc
-- "\n<html>\n    <body>\n        <p>Testing string</p>\n    </body>\n</html>"