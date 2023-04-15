#lang racket
(require 2htdp/batch-io)

;; Regex to find keywords
(define keywords #rx"(?<![A-Za-z0-9])(using|namespace|public|class|private|readonly|int|char|float|static|this|out|if|else|for|while|string|return|foreach|bool|do|enum|is|new|struct|protected|trow|goto|override|uint|virtual|void|double|case|const|ulong|short|try|catch)(?![A-Za-z0-9])")

;; Regex to find string literals
(define literals #rx"\"[^\"]*\"")

;; Regex to find delimeters (), [], {}
(define delimiters #rx"(\\(|\\)|\\(\\))|(\\[|\\]|\\[\\])|(\\{|\\}|\\{\\})")

;; Regex to find operators
(define operators #rx" (\\+|-|\\*|/|%|=|==|\\+=|-=|\\*=|/=|\\+\\+|--||||&&|!|<|>|<=|>=) ")

;; Regex for function and method calls
(define functions #rx"(?<= |\\.)[A-Za-z0-9_ ]+(?=\\()")

;; Regex to find block comments
(define blockComments #rx"/\\*.*\\*/")

;; iterateLine (list of string, function to apply to each line) => string
;; the function traverses the array, applies foo to each element and concatenates
;; them all into a string with <br> tags on the line breaks
(define iterateLine
    (lambda (lst foo)
        (cond
            [(empty? lst) ""]
            [(empty? (cdr lst)) (foo (car lst))]
            [else (string-append (foo (car lst)) " <br> " (iterateLine (cdr lst)  foo))])))

;; We load the input file
(define inputFile (read-file "input.cs"))

;; We replace the block comments with a comment tag and its content
(define lines
    (string-split (regexp-replace* blockComments inputFile "<span title='c'>&</span>") "\n"))

;; (string) => string 
;; takes a cs string, creates the tags for the comments and adds tab separation
(define comments
    (lambda (str)
    (regexp-replace* #rx"    " 
        (regexp-replace* #rx"//.*$" str "<span title='c'>&</span>")
        "\\&emsp;")))

;; The now commented lines of code
(define commented
    (iterateLine lines comments))

;; Replacing the key words with purple tags
(define kw
    (regexp-replace* keywords commented "<span title='k'>&</span>"))

;; Replacing the string literals with green tags
(define lt
    (regexp-replace* literals kw "<span title='s'>&</span>"))

;; Replacing the functions and method calls with blue tags
(define fn
    (regexp-replace* functions lt "<span title='f'>&</span>"))

;; Replacing the delimiters with yellow tags
(define del 
    (regexp-replace* delimiters fn "<span title='p'>&</span>"))

;; Replacing the operators with pink tags
(define op
    (regexp-replace* operators del "<span title='o'>&</span>"))


;; Final string of html text
(define html op)

;; Style tag
(define style 
"<style>*{font-family:'Courier New',Courier,monospace;background-color:#151220;color:#7fffd4}[title~=\"k\"]{color:#a403c4}[title~=\"f\"]{color:#00f}[title~=\"p\"]{color:#ffff47}[title~=\"s\"]{color:#adff2f}[title~=\"c\"]{color:#bbbbbb}[title~=\"o\"]{color:palevioletred;}</style>"
)

;; We append the style with the html and then it is written in output.html
(write-file "output.html" (string-append html style))
