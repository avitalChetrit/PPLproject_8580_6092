;;By: Tal Shezifi 213878580, Avital Hazan 214086092
;;Practice group number 150060.21.5786.42
(ns tar0.stage5
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
;; --- State Management Infrastructure: Symbol Table & VM Writer ---
;; =================================================================================

(defn create-context [tokens writer]
  (atom {:tokens tokens ;; List of tokens remaining for analysis.
         :writer writer ;; The Writer object is open for writing to the VM file
         :class-name nil;; will keep the current class name
         ;; --- Symbol Tables ---
         :class-table {} ;; Class-level table
         :subroutine-table {};; Function/Method Level Table
         ;; Running counters for assigning unique indexes (starting from 0)
         :indices {"static" 0 "field" 0 "argument" 0 "local" 0}
         ;; Internal counters to produce unique labels (if and while)
         :label-counts {"if" 0 "while" 0}}))

;; Peeks at the first token at the top of the list without removing it (Lookahead)
(defn peek-token [ctx]
  (first (:tokens @ctx)))

(defn next-token! [ctx]
  (let [t (peek-token ctx)];; Saves the current token at the top of the list
    (swap! ctx update :tokens rest);; Updates the context and advances the flow by removing the first token (rest)
    t))

;;Reset the function table
(defn reset-subroutine-table! [ctx]
  (swap! ctx assoc :subroutine-table {});; At the beginning of each subroutine, the function scope (Method Scope) must be cleared.
  (swap! ctx update :indices assoc "argument" 0 "local" 0));; Reset counters of arguments and local variables back to 0 (for the new function)

;;Adding a variable to a table
(defn add-symbol! [ctx name type kind]
  ;; 1. Retrieve the current free index for the variable type (kind)
  (let [current-idx (get-in @ctx [:indices kind])
        ;; 2. Construct the variable record as required: Kind, Type, and Index
        entry {:type type :kind kind :index current-idx}
        ;; 3. Determine the save destination: static/field goes to the class, argument/local to the function
        table-key (if (#{"static" "field"} kind) :class-table :subroutine-table)]
    ;; 4. Insert the new variable into the appropriate table under its name
    (swap! ctx assoc-in [table-key name] entry)
    ;; 5. Advance the running counter of the same Kind by 1 for the next variable to come
    (swap! ctx update-in [:indices kind] inc)))

;;Variable search in tables
(defn lookup-symbol [ctx name]
  ;; Scoping rule: First search the function's local table.
  ;; If the variable does not exist there (returns nil), the 'or' function will continue searching the global class table.
  (or (get-in @ctx [:subroutine-table name])
      (get-in @ctx [:class-table name])))

;;Unique Label Generator
(defn gen-label! [ctx type-prefix]
  ;; Retrieve the current counter of the requested label type (if or while)
  (let [current-count (get-in @ctx [:label-counts type-prefix])]
    ;; Advance the internal counter in the context by 1.
    (swap! ctx update-in [:label-counts type-prefix] inc)
    ;; Returns the label string in standard format in uppercase (e.g.: IF_0, WHILE_3)
    (str (str/upper-case type-prefix) "_" current-count)))

;;Direct writing to the VM file.
(defn write-vm! [ctx line]
  ;;Writing the VM command with a line feed
  (.write (:writer @ctx) (str line "\n")))

;;Push and pull commands
(defn write-push! [ctx segment index]
  ;; Field mapping rule: A field variable in Jack is translated to a "this" segment in VM
  (let [seg (if (= segment "field") "this" segment)]
    ;; Generate a formal push command in VM
    (write-vm! ctx (str "push " seg " " index))))

(defn write-pop! [ctx segment index]
  (let [seg (if (= segment "field") "this" segment)]
    ;; Generate a formal pop command in VM
    (write-vm! ctx (str "pop " seg " " index))))

;;Generating arithmetic commands
(defn write-arithmetic! [ctx command]
  ;; accepts an arithmetic command (add, sub, neg, not) and prints it directly on its own line
  (write-vm! ctx command))

;; =================================================================================
;; --- Parsing Engine & Compilation Engine ---
;; =================================================================================

;; Class analysis
(defn compile-class [ctx]
  (next-token! ctx) ;; Swallows the reserved word 'class'
  (let [c-name (next-token! ctx)];; Swallows the department name
    (swap! ctx assoc :class-name c-name));; Saves the class name in the context
  (next-token! ctx)  ;; swallows '{'

  ;; As long as the next token is a class-level variable definition (static or field), parse the line
  (while (#{"static" "field"} (peek-token ctx))
    (compile-class-var-dec ctx))
  ;; As long as the next token is a subroutine definition (constructor, function, or method), parse it
  (while (#{"constructor" "function" "method"} (peek-token ctx))
    (compile-subroutine ctx))

  (next-token! ctx))  ;; swallows'}' and end the class

;;Defining class variables
(defn compile-class-var-dec [ctx]
  (let [kind (next-token! ctx);; Swallows the variable type ('static' or 'field')
    type (next-token! ctx);; Swallows the type ('int', 'char', 'boolean' or class name)
    name (next-token! ctx)];; Swallows the first variable name
(add-symbol! ctx name type kind);; Adds the variable to the class symbol table
;; Handling a comma-separated list of variables on the same line (e.g.: field int x, y, z;)
(while (= "," (peek-token ctx))
  (next-token! ctx);; swallows the comma ','
  (add-symbol! ctx (next-token! ctx) type kind));; Swallows the next variable name and adds it to the table
(next-token! ctx))) ;;swallows ';'

;;Function/Method/Constructor Analysis
(defn compile-subroutine [ctx]
  (reset-subroutine-table! ctx);; Rule: Resets the local symbol table and counters on entry to a new function
  (let [subroutine-kind (next-token! ctx);; Swallows the subroutine type ('constructor', 'function', 'method')
        return-type (next-token! ctx);; Swallows the return type (void or data type)
        subroutine-name (next-token! ctx)];; Swallows the subroutine name

    ;; Method rule: If this is a method, the first argument (index 0) must be the object itself ("this")
    (when (= subroutine-kind "method")
      (add-symbol! ctx "this" (:class-name @ctx) "argument"))

    (next-token! ctx) ;; Swallows'('
    (compile-parameter-list ctx);; Parses the parameter list and inserts them into the symbol table
    (next-token! ctx) ;; ')'

    (next-token! ctx) ;; '{'
    ;; As long as there are declarations of local variables (var), parse them and insert them into the table
    (while (= "var" (peek-token ctx))
      (compile-var-dec ctx))

    ;; Generating the function definition command in the VM: function ClassName.SubroutineName NumOfLocals
    (let [num-locals (get-in @ctx [:indices "local"])
          full-subroutine-name (str (:class-name @ctx) "." subroutine-name)]
      (write-vm! ctx (str "function " full-subroutine-name " " num-locals)))

    ;; If this is a constructor: allocate memory in the heap for the fields of the new object
    (cond
      (= subroutine-kind "constructor");; Pushes the number of fields to be allocated
      (let [num-fields (get-in @ctx [:indices "field"])]
        (write-push! ctx "constant" num-fields);; Pushes the number of fields to be allocated
        (write-vm! ctx "call Memory.alloc 1");; Calls the operating system to allocate memory
        (write-pop! ctx "pointer" 0));; Anchors the current object base into pointer 0 (this segment)
      ;; If this is a method: anchor the object received as argument 0 to be the current 'this'
      (= subroutine-kind "method")
      (do
        (write-push! ctx "argument" 0);; Pushes the first argument (the object pointer)
        (write-pop! ctx "pointer" 0)));; casts to pointer 0 so that field references work on the correct object

    (compile-statements ctx);; Parses and generates code for all commands within the function body
    (next-token! ctx))) ;; '}'

(defn compile-parameter-list [ctx]
  ;; Check if the parameter list is not empty (i.e., the next token is not a closing parenthesis)
  (when-not (= ")" (peek-token ctx))
    ;; Extract the type and name of the first parameter from the token stream
    (let [type (next-token! ctx)
          name (next-token! ctx)]
      ;; Register the first parameter in the subroutine symbol table as an 'argument'
      (add-symbol! ctx name type "argument")
      ;; Process additional parameters separated by commas (e.g., int x, char c)
      (while (= "," (peek-token ctx))
        (next-token! ctx) ;; Consume the comma separator ','
        ;; Extract the type and name of the next parameter in the list
        (let [next-type (next-token! ctx)
              next-name (next-token! ctx)]
          ;; Register the additional parameter into the subroutine symbol table
          (add-symbol! ctx next-name next-type "argument"))))))

(defn compile-var-dec [ctx]
  (next-token! ctx) ;; Consume 'var' keyword
  (let [type (next-token! ctx)
        name (next-token! ctx)]
    ;; Register the first local variable in the symbol table
    (add-symbol! ctx name type "local")
    ;; Process multiple variables declared on the same line separated by commas (e.g., var int x, y;)
    (while (= "," (peek-token ctx))
      (next-token! ctx) ;; Consume ','
      (add-symbol! ctx (next-token! ctx) type "local")) ;; Register subsequent variables
    (next-token! ctx))) ;; Consume the trailing ';'

(defn compile-statements [ctx]
  ;; Continually parse statements as long as the next token matches a valid Jack statement keyword
  (while (#{"let" "if" "while" "do" "return"} (peek-token ctx))
    (case (peek-token ctx)
      "let"    (compile-let ctx)
      "if"     (compile-if ctx)
      "while"  (compile-while ctx)
      "do"     (compile-do ctx)
      "return" (compile-return ctx))))

(defn compile-let [ctx]
  (next-token! ctx) ;; Consume 'let' keyword
  (let [var-name (next-token! ctx)
        sym (lookup-symbol ctx var-name)
        is-array (= "[" (peek-token ctx))]

    ;; Check if we are handling an array assignment (e.g., let arr[i] = expr;)
    (if is-array
      (do
        ;; Push array base address
        (write-push! ctx (:kind sym) (:index sym))
        (next-token! ctx) ;; Consume '['
        (compile-expression ctx)
        (next-token! ctx) ;; Consume ']'
        ;; Calculate target element address: base address + index offset
        (write-arithmetic! ctx "add")

        (next-token! ctx) ;; Consume '='
        (compile-expression ctx)

        ;; Use temp segment to safely assign the calculated value to the array element address via pointer 1
        (write-pop! ctx "temp" 0)
        (write-pop! ctx "pointer" 1)
        (write-push! ctx "temp" 0)
        (write-pop! ctx "that" 0))

      ;; Regular variable assignment (e.g., let x = expr;)
      (do
        (next-token! ctx) ;; Consume '='
        (compile-expression ctx)
        ;; Pop the evaluated value directly into the variable's memory segment
        (write-pop! ctx (:kind sym) (:index sym))))
    (next-token! ctx))) ;; Consume ';'

(defn compile-if [ctx]
  ;; Generate three distinct labels to support code jumping for conditional branching
  (let [label-true (gen-label! ctx "if")
        label-false (gen-label! ctx "if")
        label-end (gen-label! ctx "if")]
    (next-token! ctx) ;; Consume 'if'
    (next-token! ctx) ;; Consume '('
    (compile-expression ctx)
    (next-token! ctx) ;; Consume ')'

    ;; Conditional flow branching using VM instructions
    (write-vm! ctx (str "if-goto " label-true))
    (write-vm! ctx (str "goto " label-false))
    (write-vm! ctx (str "label " label-true))

    (next-token! ctx) ;; Consume '{'
    (compile-statements ctx)
    (next-token! ctx) ;; Consume '}'

    ;; Check if an optional 'else' clause exists
    (if (= "else" (peek-token ctx))
      (do
        (write-vm! ctx (str "goto " label-end))
        (write-vm! ctx (str "label " label-false))
        (next-token! ctx) ;; Consume 'else'
        (next-token! ctx) ;; Consume '{'
        (compile-statements ctx)
        (next-token! ctx) ;; Consume '}'
        (write-vm! ctx (str "label " label-end)))
      (write-vm! ctx (str "label " label-false)))))

(defn compile-while [ctx]
  ;; Generate unique labels for looping back and for breaking out of the loop
  (let [label-exp (gen-label! ctx "while")
        label-end (gen-label! ctx "if")]
    (write-vm! ctx (str "label " label-exp)) ;; Label marking the start of expression evaluation
    (next-token! ctx) ;; Consume 'while'
    (next-token! ctx) ;; Consume '('
    (compile-expression ctx)
    (next-token! ctx) ;; Consume ')'

    ;; Negate condition; if false (zero), jump out of the loop body
    (write-arithmetic! ctx "not")
    (write-vm! ctx (str "if-goto " label-end))

    (next-token! ctx) ;; Consume '{'
    (compile-statements ctx)
    (next-token! ctx) ;; Consume '}'

    (write-vm! ctx (str "goto " label-exp)) ;; Loop back to re-evaluate condition
    (write-vm! ctx (str "label " label-end)))) ;; Exit label

(defn compile-do [ctx]
  (next-token! ctx) ;; Consume 'do' keyword
  (let [first-id (next-token! ctx)]
    (cond
      ;; Case A: Direct method call on the current object instance (e.g., do print();)
      (= "(" (peek-token ctx))
      (do
        (write-push! ctx "pointer" 0) ;; Pass 'this' as the implicit first argument
        (next-token! ctx) ;; Consume '('
        (let [n-args (compile-expression-list ctx)]
          (next-token! ctx) ;; Consume ')'
          ;; Call routine with class prefix and increment argument count to account for 'this'
          (write-vm! ctx (str "call " (:class-name @ctx) "." first-id " " (inc n-args)))))

      ;; Case B: Qualified subroutine call via dot notation (e.g., do Math.sqrt() or do obj.method())
      (= "." (peek-token ctx))
      (do
        (next-token! ctx) ;; Consume '.'
        (let [subroutine-name (next-token! ctx)
              sym (lookup-symbol ctx first-id)]
          (next-token! ctx) ;; Consume '('
          (if sym
            ;; If the identifier exists in the symbol table, it is an object method invocation
            (do
              (write-push! ctx (:kind sym) (:index sym)) ;; Pass object reference as first argument
              (let [n-args (compile-expression-list ctx)]
                (next-token! ctx) ;; Consume ')'
                (write-vm! ctx (str "call " (:type sym) "." subroutine-name " " (inc n-args)))))
            ;; If not found, it is a static function call belonging to another Class
            (let [n-args (compile-expression-list ctx)]
              (next-token! ctx) ;; Consume ')'
              (write-vm! ctx (str "call " first-id "." subroutine-name " " n-args))))))))

  (next-token! ctx) ;; Consume ';'
  (write-pop! ctx "temp" 0)) ;; 'do' statements ignore return values; pop output into temp segment to discard it

(defn compile-return [ctx]
  (next-token! ctx) ;; Consume 'return' keyword
  ;; If it's a void return, push a dummy 0 value onto the stack per Jack specs
  (if (= ";" (peek-token ctx))
    (write-push! ctx "constant" 0)
    (compile-expression ctx)) ;; Evaluate return expression
  (next-token! ctx) ;; Consume ';'
  (write-vm! ctx "return"))

(defn compile-expression [ctx]
  (compile-term ctx) ;; Compile the first operand term
  ;; Loop to handle sequential binary operations matching operational precedence signs
  (while (and (peek-token ctx)
              (= 1 (count (peek-token ctx)))
              (op-symbols (first (peek-token ctx))))
    (let [op (next-token! ctx)]
      (compile-term ctx) ;; Compile right-hand operand term
      ;; Emit corresponding VM command or OS method call depending on the operator symbol
      (case op
        "+" (write-arithmetic! ctx "add")
        "-" (write-arithmetic! ctx "sub")
        "*" (write-vm! ctx "call Math.multiply 2")
        "/" (write-vm! ctx "call Math.divide 2")
        "&" (write-arithmetic! ctx "and")
        "|" (write-arithmetic! ctx "or")
        "<" (write-arithmetic! ctx "lt")
        ">" (write-arithmetic! ctx "gt")
        "=" (write-arithmetic! ctx "eq")))))

(defn compile-term [ctx]
  (let [next-t (peek-token ctx)
        ;; classify-token returns a pair: [token_tag, clean_value]
        [token-tag token-val] (classify-token next-t)]
    (cond
      ;; 1. Handling unary operators (negation or minus)
      (#{"-" "~"} next-t)
      (do
        (next-token! ctx)
        (compile-term ctx)
        (if (= next-t "-")
          (write-arithmetic! ctx "neg")
          (write-arithmetic! ctx "not")))

      ;; 2. Handling expressions enclosed in parentheses ( Expression )
      (= "(" next-t)
      (do
        (next-token! ctx) ;; Consumes '('
        (compile-expression ctx)
        (next-token! ctx)) ;; Consumes ')'

      ;; 3. Integer constants
      (= token-tag :integerConstant)
      (do
        (next-token! ctx) ;; Consumes the token from the stream
        (write-push! ctx "constant" token-val)) ;; Uses the valid numeric value

      ;; 4. String constants
      (= token-tag :stringConstant)
      (let [_ (next-token! ctx) ;; Consumes the raw token from the stream
            len (count token-val)] ;; token-val is already the clean string without quotes!
        (write-push! ctx "constant" len)
        (write-vm! ctx "call String.new 1")
        (doseq [ch token-val]
          (write-push! ctx "constant" (int ch))
          (write-vm! ctx "call String.appendChar 2")))

      ;; 5. Built-in constants (false, null)
      (#{"false" "null"} next-t)
      (do (next-token! ctx) (write-push! ctx "constant" 0))

      ;; 6. Built-in constant true
      (= "true" next-t)
      (do (next-token! ctx) (write-push! ctx "constant" 0) (write-arithmetic! ctx "not"))

      ;; 7. Reserved word 'this'
      (= "this" next-t)
      (do (next-token! ctx) (write-push! ctx "pointer" 0))

      ;; 8. Handling identifiers: variables, arrays, and function calls
      :else
      (let [first-id (next-token! ctx)
            lookahead (peek-token ctx)]
        (cond
          ;; a. Array element access: arr[expression]
          (= "[" lookahead)
          (let [sym (lookup-symbol ctx first-id)]
            (write-push! ctx (:kind sym) (:index sym))
            (next-token! ctx) ;; Consumes '['
            (compile-expression ctx)
            (next-token! ctx) ;; Consumes ']'
            (write-arithmetic! ctx "add")
            (write-pop! ctx "pointer" 1)
            (write-push! ctx "that" 0))

          ;; b. Implicit function/method call inside an expression
          (#{"(" "."} lookahead)
          (do
            (next-token! ctx) ;; Consumes '.' or '('
            (if (= "(" lookahead)
              (do
                (write-push! ctx "pointer" 0)
                (let [n-args (compile-expression-list ctx)]
                  (next-token! ctx) ;; Consumes ')'
                  (write-vm! ctx (str "call " (:class-name @ctx) "." first-id " " (inc n-args)))))
              (let [subroutine-name (next-token! ctx)]
                (next-token! ctx) ;; Consumes '(' following the subroutine name
                (let [sym (lookup-symbol ctx first-id)]
                  (if sym
                    (do
                      (write-push! ctx (:kind sym) (:index sym))
                      (let [n-args (compile-expression-list ctx)]
                        (next-token! ctx) ;; Consumes ')'
                        (write-vm! ctx (str "call " (:type sym) "." subroutine-name " " (inc n-args)))))
                    (let [n-args (compile-expression-list ctx)]
                      (next-token! ctx) ;; Consumes ')'
                      (write-vm! ctx (str "call " first-id "." subroutine-name " " n-args))))))))

          ;; c. Regular variable (local, argument, or field)
          :else
          (let [sym (lookup-symbol ctx first-id)]
            (write-push! ctx (:kind sym) (:index sym))))))))

(defn compile-expression-list [ctx]
  (let [arg-count (atom 0)]
    ;; Check if the expression list is empty (i.e., immediate closing parenthesis)
    (when-not (= ")" (peek-token ctx))
      (compile-expression ctx) ;; Compile first argument expression
      (swap! arg-count inc)
      ;; Loop through subsequent expression arguments separated by commas
      (while (= "," (peek-token ctx))
        (next-token! ctx) ;; Consume ',' delimiter
        (compile-expression ctx) ;; Compile next argument expression
        (swap! arg-count inc)))
    @arg-count)) ;; Return total count of processed expressions/arguments

;; --- Main Execution Function ---
(defn -main [& args]
  ;; Retrieve directory path argument from command line parameters or prompt user interactively
  (let [raw-path (or (first args)
                     (do (println "Please enter the directory path:")
                         (read-line)))
        path (str/trim (str raw-path))
        dir (io/file path)]
    ;; Validate specified file path existence
    (if (.exists dir)
      ;; Check whether target is a directory or single standalone file, then collect target .jack files
      (let [files (if (.isDirectory dir)
                    (filter #(str/ends-with? (.getName %) ".jack") (.listFiles dir))
                    [dir])]
        ;; Notify if directory contains no valid source code components
        (if (empty? files)
          (println "No .jack files found in the directory.")
          ;; Process source compilation incrementally for each discovered target source file
          (doseq [f files]
            (let [file-content (slurp f)
                  tokens (tokenize file-content)
                  ;; Construct equivalent .vm destination file naming blueprint
                  output-path (str (str/replace (.getAbsolutePath f) #"\.jack$" "") ".vm")]
              ;; Open resource stream instance for file manipulation tasks
              (with-open [writer (io/writer output-path)]
                (let [ctx (create-context tokens writer)]
                  (compile-class ctx))) ;; Ignite compiler parser sequencing
              (println "Created VM Code:" (.getName (io/file output-path)))))))
      (println "Error: Directory not found. Checked path: [" path "]"))))