;;By: Tal Shezifi 213878580, Avital Hazan 214086092
;;Practice group number 150060.21.5786.42
;; Define the namespace of the file.
(ns tar0.lexical
  ;; Imports Java I/O utilities for file handling
  (:require [clojure.java.io :as io]
    ;; Imports string manipulation functions
            [clojure.string :as str])
  ;; Allows running as standalone application
  (:gen-class))

;; Forward declaration of the function so it can be referenced before its physical definition.
(declare classify-token)
;; --- Pre-declarations of the new Parser functions so they can call each other recursively ---
(declare compile-class compile-class-var-dec compile-subroutine
         compile-parameter-list compile-subroutine-body compile-var-dec
         compile-statements compile-let compile-if compile-while compile-do
         compile-return compile-expression compile-term compile-expression-list)

;; --- Jack language settings ---

;; Define a set of all reserved keywords in the Jack language
(def keywords #{"class" "constructor" "function" "method" "field" "static" "var"
                "int" "char" "boolean" "void" "true" "false" "null" "this"
                "let" "do" "if" "else" "while" "return"})

;; Define a set of all allowed symbols in the Jack language
(def symbols #{\{ \} \( \) \[ \] \. \, \; \+ \- \* \/ \& \| \< \> \= \~})

;; A dedicated set of operators for the Parser to recognize expressions
(def op-symbols #{\+ \- \* \/ \& \| \< \> \=})

;; --- Handling special characters for XML ---
;; Helper function to convert problematic characters to valid XML format
(defn escape-xml [s]
  (-> s
      (str/replace "&" "&amp;");; for example replace & with its XML entity.
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

;; --- Tokenizing ---
;; Function responsible for breaking raw text into a list of tokens (individual words)
(defn tokenize [input]
  (let [
        ;; Step A: Remove block comments (/*...*/) and line comments (//...)
        clean-code (-> input
                       (str/replace #"(?s)/\*.*?\*/" " ")
                       (str/replace #"//.*" " "))
        ;; Step B: Define a Regular Expression (Regex) to find strings, identifiers, numbers, or symbols
        token-pattern #"\"[^\"]*\"|[a-zA-Z_][a-zA-Z0-9_]*|[0-9]+|[\{\}\(\)\[\]\.\,\;\+\-\*\/\&\|\<\>\=\~]"]
    ;; Scan the clean code and return a list of everything that matches the Regex pattern
    (re-seq token-pattern clean-code)))

;; Function that takes a list of tokens and writes them to an XML file
(defn write-token-xml [tokens writer]
  ;; Write the XML opening tag
  (.write writer "<tokens>\n")
  ;; Loop through each token in the list
  (doseq [t tokens]
    ;; Split token into type (tag) and content (value)
    (let [[tag value] (classify-token t)
          ;; Escape content for safe XML formatting
          final-value (escape-xml value)]
      ;; Write the line in format: <type> value </type>
      (.write writer (str "<" (name tag) "> " final-value " </" (name tag) ">\n"))))
  ;; Write the XML closing tag
  (.write writer "</tokens>\n"))

;; Function that categorizes each token into its type (keyword, integer, string, etc.)
(defn classify-token [t]
  (cond
    ;; Check if the token exists in the keywords set
    (keywords t) [:keyword t]

    ;; Check if the token is a sequence of digits (number)
    (re-matches #"[0-9]+" t)
    (let [n (Integer/parseInt t)]
      (if (<= 0 n 32767)   ;; Verify the number is within the allowed range per project requirements
        [:integerConstant t]
        [:identifier t]))  ;; If out of range, treat it as an identifier (as per Jack specs)
    ;; Check if the token starts with quotes (string)
    (str/starts-with? t "\"")
    [:stringConstant (subs t 1 (dec (count t)))]  ;; Return the string without the actual quotes

    ;; Check if the token is a single character that exists in the symbols set
    (and (= 1 (count t)) (symbols (first t)))
    [:symbol t]
    ;; In any other case, the token is considered an identifier (variable name, function name, etc.)
    :else
    [:identifier t]))


;; =================================================================================
;; --- Parser / Compilation Engine ---;;
;; =================================================================================
;; Function that initializes the parser's state.
(defn create-context [tokens writer]
  (atom {:tokens tokens :writer writer :indent 0}))

;; Function that peeks at the first token currently at the head of the token list, without removing it or advancing the list.
(defn peek-token [ctx]
  (first (:tokens @ctx)))

;; Function that extracts the current token from the head of the list, while simultaneously updating the Atom (via swap!) to hold the remaining tokens (rest).
(defn next-token! [ctx]
  (let [t (peek-token ctx)]
    (swap! ctx update :tokens rest)
    t))

;; Private helper function that generates a string of spaces corresponding to the current indentation depth (:indent) stored in the Context.
(defn- get-spaces [ctx]
  (apply str (repeat (:indent @ctx) " ")))

;; A function that writes a single line to a file with appropriate indentation and line feed.
(defn write-line! [ctx content]
  (let [writer (:writer @ctx)
        spaces (get-spaces ctx)]
    (.write writer (str spaces content "\n"))))
;; Function used to open a structural/composite XML tag (such as <class> or <statements>),and increases the indent by 2 spaces.
(defn open-tag! [ctx tag-name]
  (write-line! ctx (str "<" tag-name ">"))
  (swap! ctx update :indent #(+ % 2)))

;; A function that reduces the indentation by 2 spaces and closes the compound tag.
(defn close-tag! [ctx tag-name]
  (swap! ctx update :indent #(- % 2))
  (write-line! ctx (str "</" tag-name ">")))

;; Writes a terminal token directly to the XML file with the correct indentation
;; It pops the next token, categorizes its type (classify-token), escapes any problematic XML characters (escape-xml), and outputs it in the format: <type> value </type>.
(defn write-terminal! [ctx]
  (let [t (next-token! ctx)
        [tag value] (classify-token t)
        final-value (escape-xml value)]
    (write-line! ctx (str "<" (name tag) "> " final-value " </" (name tag) ">"))))

;; --- Recursive functions for parsing the grammar of the language ---

;; Function that parses and compiles an entire class definition. The top-level rule in the Jack grammar.
(defn compile-class [ctx]
  ;; Opens the main class tag: <class>
  (open-tag! ctx "class")
  ;; Writes the 'class' keyword terminal
  (write-terminal! ctx)
  ;; Writes the class identifier name (className)
  (write-terminal! ctx)
  ;; Writes the opening curly brace terminal '{'
  (write-terminal! ctx)

  ;; Run on ... (if any)
  (while (#{"static" "field"} (peek-token ctx))
    (compile-class-var-dec ctx))

  (while (#{"constructor" "function" "method"} (peek-token ctx))
    (compile-subroutine ctx))

  ;; Writes the closing curly brace terminal '}'
  (write-terminal! ctx)
  ;; Closes the main class tag: </class>
  (close-tag! ctx "class"))

(defn compile-class-var-dec [ctx]
  (open-tag! ctx "classVarDec");; Opens the tag: <classVarDec>
  (write-terminal! ctx) ;; 'static' | 'field'
  (write-terminal! ctx) ;; type (int, boolean, char, className)
  (write-terminal! ctx) ;; varName
  ;; Loop that handles multi-variable inline declarations separated by commas (e.g., static int x, y, z;)
  (while (= "," (peek-token ctx))
    (write-terminal! ctx) ;; ','
    (write-terminal! ctx)) ;; varName
  (write-terminal! ctx) ;; ';'
  (close-tag! ctx "classVarDec"));; Closes the tag: </classVarDec>

;; Function that parses subroutines (functions, methods, or constructors).
(defn compile-subroutine [ctx]
  (open-tag! ctx "subroutineDec")
  (write-terminal! ctx) ;; 'constructor' | 'function' | 'method'
  (write-terminal! ctx) ;; 'void' | type
  (write-terminal! ctx) ;; subroutineName
  (write-terminal! ctx) ;; '('
  (compile-parameter-list ctx);; Invokes parameter list parser (can be empty)
  (write-terminal! ctx) ;; ')'
  (compile-subroutine-body ctx);; Invokes the subroutine body parser
  (close-tag! ctx "subroutineDec"))

;; Function that parses a subroutine's parameter list (enclosed within the parentheses).
(defn compile-parameter-list [ctx]
  (open-tag! ctx "parameterList");; Opens the tag: <parameterList>
  ;; Conditional check: if the next token is not ')', it implies the list contains at least one parameter
  (when-not (= ")" (peek-token ctx))
    (write-terminal! ctx) ;; type
    (write-terminal! ctx) ;; varName
    ;; Loop that handles all additional parameters separated by commas (e.g., int x, char y)
    (while (= "," (peek-token ctx))
      (write-terminal! ctx) ;; ','
      (write-terminal! ctx) ;; type
      (write-terminal! ctx))) ;; varName
  (close-tag! ctx "parameterList"));; Closes the tag: </parameterList>

;; Function that parses a subroutine's body (the statements enclosed in curly brackets).
(defn compile-subroutine-body [ctx]
  (open-tag! ctx "subroutineBody");; Opens the tag: <subroutineBody>
  (write-terminal! ctx) ;; '{'
  ;; Loop that executes as long as there are local variable declarations at the start of the block ('var' keyword)
  (while (= "var" (peek-token ctx))
    (compile-var-dec ctx));; Invokes local variable declaration parser
  (compile-statements ctx);; Invokes statement sequence parser for the code within the block
  (write-terminal! ctx) ;; '}'
  (close-tag! ctx "subroutineBody"));; Closes the tag: </subroutineBody>

;; Function that parses local variable declarations (commencing with the 'var' keyword).
(defn compile-var-dec [ctx]
  (open-tag! ctx "varDec");; Opens the tag: <varDec>
  (write-terminal! ctx) ;; 'var'
  (write-terminal! ctx) ;; type(int, char...)
  (write-terminal! ctx) ;; Writes the variable's name identifier (varName)
  ;; Loop that handles multi-variable inline local declarations separated by commas (e.g., var int a, b, c;)
  (while (= "," (peek-token ctx))
    (write-terminal! ctx) ;; ','
    (write-terminal! ctx)) ;; Writes the next variable's name identifier
  (write-terminal! ctx) ;; ';'
  (close-tag! ctx "varDec"));; Closes the tag: </varDec>

;; Function that parses a sequence of zero or more executable statements.
(defn compile-statements [ctx]
  (open-tag! ctx "statements") ;; Opens the tag: <statements>
  ;; Loop that executes as long as the next token belongs to the set of valid statement keyword markers in Jack
  (while (#{"let" "if" "while" "do" "return"} (peek-token ctx))
    (case (peek-token ctx)
      "let"    (compile-let ctx);; Routes to assignment statement parser (let)
      "if"     (compile-if ctx)
      "while"  (compile-while ctx)
      "do"     (compile-do ctx)
      "return" (compile-return ctx)))
  (close-tag! ctx "statements"));; Closes the tag: </statements>

;; Function that parses an assignment statement (let statement).
(defn compile-let [ctx]
  (open-tag! ctx "letStatement"); Opens the tag: <letStatement>
  (write-terminal! ctx) ;; 'let'
  (write-terminal! ctx) ;; Writes the receiving target variable identifier (varName)
  ;; Conditional block checking if an array cell is accessed via square brackets (e.g., let arr[5] = 1;)
  (when (= "[" (peek-token ctx))
    (write-terminal! ctx) ;; '['
    (compile-expression ctx);; Evaluates the array's index expression inside the brackets
    (write-terminal! ctx)) ;; ']'
  (write-terminal! ctx) ;; '='
  (compile-expression ctx);; Evaluates the right-hand side source value expression
  (write-terminal! ctx) ;; ';'
  (close-tag! ctx "letStatement"));; Closes the tag: </letStatement>

;; Function that parses conditional branching blocks (if statements), handling optional else segments.
(defn compile-if [ctx]
  (open-tag! ctx "ifStatement");; Opens the tag: <ifStatement>
  (write-terminal! ctx) ;; 'if'
  (write-terminal! ctx) ;; '('
  (compile-expression ctx);; Parses the boolean condition expression
  (write-terminal! ctx) ;; ')'
  (write-terminal! ctx) ;; '{'
  (compile-statements ctx);; Parses statements contained in the execution body of the 'if'
  (write-terminal! ctx) ;; '}'
  ;; Structural lookup checking if the keyword 'else' directly follows the closed block
  (when (= "else" (peek-token ctx))
    (write-terminal! ctx) ;; 'else'
    (write-terminal! ctx) ;; '{'
    (compile-statements ctx) ;; Parses statements contained in the execution body of the 'else'
    (write-terminal! ctx)) ;; '}'
  (close-tag! ctx "ifStatement"));; Closes the tag: </ifStatement>

;; Function that parses condition-driven loop blocks (while statements).
(defn compile-while [ctx]
  (open-tag! ctx "whileStatement");; Opens the tag: <whileStatement>
  (write-terminal! ctx) ;; 'while'
  (write-terminal! ctx) ;; '('
  (compile-expression ctx);; Parses the boolean tracking condition expression
  (write-terminal! ctx) ;; ')'
  (write-terminal! ctx) ;; '{'
  (compile-statements ctx);; Parses the statements looping inside the body block
  (write-terminal! ctx) ;; '}'
  (close-tag! ctx "whileStatement"));; Closes the tag: </whileStatement>

;; Function that parses independent void-subroutine invocations (do statements).
(defn compile-do [ctx]
  (open-tag! ctx "doStatement");; Opens the tag: <doStatement>
  (write-terminal! ctx) ;; 'do'
  (write-terminal! ctx) ;; subroutineName | className | varName
  ;; Evaluates lookahead to resolve the structural configuration of the Subroutine Call:
  (cond
    ;; Case A: Direct method invocation on the local instance scope (e.g., do draw();)
    (= "(" (peek-token ctx))
    (do
      (write-terminal! ctx) ;; '('
      (compile-expression-list ctx);; Parses the parameters sent as arguments
      (write-terminal! ctx)) ;; ')'
    ;; Case B: External invocation routed through another class or instance object (e.g., do Screen.drawCircle();)
    (= "." (peek-token ctx))
    (do
      (write-terminal! ctx) ;; '.'
      (write-terminal! ctx) ;; subroutineName
      (write-terminal! ctx) ;; '('
      (compile-expression-list ctx);; Parses the expressions sent as argument payloads
      (write-terminal! ctx))) ;; ')'
  (write-terminal! ctx) ;; ';'
  (close-tag! ctx "doStatement"));; Closes the tag: </doStatement>

;; Function that parses subroutine termination paths (return statements).
(defn compile-return [ctx]
  (open-tag! ctx "returnStatement");; Opens the tag: <returnStatement>
  (write-terminal! ctx) ;; 'return'
  ;; Conditional branch: if the trailing token is not a semicolon ';', it implies a return value expression is attached
  (when-not (= ";" (peek-token ctx))
    (compile-expression ctx));; Parses the evaluated expression returned by the path
  (write-terminal! ctx) ;; ';'
  (close-tag! ctx "returnStatement"));; Closes the tag: </returnStatement>

;; Function that parses an Expression. Composed of an initial core element (term), followed optionally by paired operation symbols and succeeding terms.
(defn compile-expression [ctx]
  (open-tag! ctx "expression");; Opens the tag: <expression>
  (compile-term ctx);; Evaluates the primary operand structure (term)
  ;; Loop running as long as subsequent lookahead matches mathematical or comparison operation symbols (+, -, *, / etc.)
  (while (and (peek-token ctx)
              (= 1 (count (peek-token ctx)))
              (op-symbols (first (peek-token ctx))))
    (write-terminal! ctx) ;; op
    (compile-term ctx))   ;; term
  (close-tag! ctx "expression"));; Closes the tag: </expression>

;; Function that parses an internal term component within an expression. Contains deep nesting options to isolate symbols, calls, arrays, and primitives.
(defn compile-term [ctx]
  (open-tag! ctx "term");; Opens the tag: <term>
  (let [next-t (peek-token ctx)];; Peeks at the token currently sitting at the front
    (cond
      ;; Case A: Handles prefix unary operators (e.g., mathematical negation or logical inversion: -, ~)
      (#{"-" "~"} next-t)
      (do
        (write-terminal! ctx) ;; unaryOp

        (compile-term ctx));; Recursive callback to analyze the attached term value

      ;; Case B: Handles arithmetic groupings encapsulated within parentheses, such as: (x + 5)
      (= "(" next-t)
      (do
        (write-terminal! ctx) ;; '('
        (compile-expression ctx);; Recursive callback to analyze the internal grouped expression block
        (write-terminal! ctx)) ;; ')'
      ;; Case C: Handles identifiers which could signify a primitive variable, an array address, or an embedded routine call
      :else
      (let [remain (:tokens @ctx)
            ;; Peeks at the secondary token (Lookahead) without mutating the queue, to differentiate the identifier classification
            lookahead (if (> (count remain) 1) (second remain) nil)]
        (cond
          ;; If the lookahead symbol equates to '[', it targets an indexed array memory offset (e.g., arr[i])
          (= "[" lookahead)
          (do
            (write-terminal! ctx) ;; Writes the class name, instance tracking object, or method identifier
            (write-terminal! ctx) ;; '['
            (compile-expression ctx);; Resolves the evaluation of the cell index value
            (write-terminal! ctx)) ;;']'

          (#{"(" "."} lookahead);; If the lookahead matches '(' or '.', it triggers an embedded subroutine call within an expression block
          (do
            (write-terminal! ctx) ;; כותב את המזהה הראשון (subroutineName / className / varName)
            (if (= "(" lookahead)
              (do
                (write-terminal! ctx) ;; '('
                (compile-expression-list ctx)
                (write-terminal! ctx)) ;; ')'
              (do
                (write-terminal! ctx) ;; '.'
                (write-terminal! ctx) ;; subroutineName
                (write-terminal! ctx) ;; '('
                (compile-expression-list ctx)
                (write-terminal! ctx)))) ;; ')'

          :else;; Case D: Basic scalar literals or simple independent tracking components (integerConstant, stringConstant, or keyword constants: true/false/null/this)
          (write-terminal! ctx)))))
  (close-tag! ctx "term"))

(defn compile-expression-list [ctx]
  (open-tag! ctx "expressionList")
  (when-not (= ")" (peek-token ctx))
    (compile-expression ctx)
    (while (= "," (peek-token ctx))
      (write-terminal! ctx) ;; ','
      (compile-expression ctx)))
  (close-tag! ctx "expressionList"))

;; --- Main Function executing on execution runtime ---
(defn -main [& args]
  ;; Get the path from arguments or directly from the user
  (let [raw-path (or (first args)
                     (do (println "Please enter the directory path:")
                         (read-line)))
        ;; Trim unnecessary whitespace from the path
        path (str/trim (str raw-path))
        dir (io/file path)]
    ;; Check if the path exists on the computer
    (if (.exists dir)
      ;; If it's a directory, take all .jack files. If it's a file, take only that file.
      (let [files (if (.isDirectory dir)
                    (filter #(str/ends-with? (.getName %) ".jack") (.listFiles dir))
                    [dir])]
        ;; If no matching files were found
        (if (empty? files)
          (println "No .jack files found in the directory.")
          ;; Loop through every found .jack file
          (doseq [f files]
            (let [file-content (slurp f) ;; Read file content
                  tokens (tokenize file-content) ;; Tokenize the content
                  ;; The current output is produced directly in the hierarchical structure of the project with the extension PH.xml
                  output-path (str (str/replace (.getAbsolutePath f) #"\.jack$" "") "PH.xml")]

              ;; Open file for writing and perform output
              (with-open [writer (io/writer output-path)]
                ;; A dedicated set of operators for the Parser to recognize expressions
                (let [ctx (create-context tokens writer)]
                  (compile-class ctx)))

              ;; Print success message to the user
              (println "Created Structure XML:" (.getName (io/file output-path)))))))
      ;; Error message if the path was not found
      (println "Error: Directory not found. Checked path: [" path "]"))))