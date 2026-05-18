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

;; --- הצהרות מראש על פונקציות ה-Parser החדשות כדי שיוכלו לקרוא אחת לשנייה ברקורסיה ---
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

;; סט אופרטורים ייעודי עבור ה-Parser לצורך זיהוי ביטויים (Expressions)
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
;; --- קוד חדש: מנוע הניתוח התחבירי (Parser / Compilation Engine) עבור חלק ב' ---
;; =================================================================================

;; ניהול המצב (State) של הטוקנים שנותרו לקריאה וה-writer הנוכחי באמצעות Atom
(defn create-context [tokens writer]
  (atom {:tokens tokens :writer writer}))

;; מציץ בטוקן הבא בתור מבלי להתקדם
(defn peek-token [ctx]
  (first (:tokens @ctx)))

;; שולף את הטוקן הבא ומתקדם צעד אחד קדימה ברשימה
(defn next-token! [ctx]
  (let [t (peek-token ctx)]
    (swap! ctx update :tokens rest)
    t))

;; כותב טוקן סופי (Terminal) ישירות לקובץ ה-XML תוך שימוש בפונקציות ה-classify וה-escape שלכן
(defn write-terminal! [ctx]
  (let [t (next-token! ctx)
        [tag value] (classify-token t)
        final-value (escape-xml value)
        writer (:writer @ctx)]
    (.write writer (str "<" (name tag) "> " final-value " </" (name tag) ">\n"))))

;; --- פונקציות רקורסיביות לניתוח הדקדוק של השפה ---

(defn compile-class [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<class>\n")
    (write-terminal! ctx) ;; 'class'
    (write-terminal! ctx) ;; className
    (write-terminal! ctx) ;; '{'

    ;; ריצה על הצהרות משתנים של המחלקה (אם קיימים)
    (while (#{"static" "field"} (peek-token ctx))
      (compile-class-var-dec ctx))

    ;; ריצה על פונקציות/מתודות (אם קיימות)
    (while (#{"constructor" "function" "method"} (peek-token ctx))
      (compile-subroutine ctx))

    (write-terminal! ctx) ;; '}'
    (.write writer "</class>\n")))

(defn compile-class-var-dec [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<classVarDec>\n")
    (write-terminal! ctx) ;; 'static' | 'field'
    (write-terminal! ctx) ;; type
    (write-terminal! ctx) ;; varName
    (while (= "," (peek-token ctx))
      (write-terminal! ctx) ;; ','
      (write-terminal! ctx)) ;; varName
    (write-terminal! ctx) ;; ';'
    (.write writer "</classVarDec>\n")))

(defn compile-subroutine [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<subroutineDec>\n")
    (write-terminal! ctx) ;; 'constructor' | 'function' | 'method'
    (write-terminal! ctx) ;; 'void' | type
    (write-terminal! ctx) ;; subroutineName
    (write-terminal! ctx) ;; '('
    (compile-parameter-list ctx)
    (write-terminal! ctx) ;; ')'
    (compile-subroutine-body ctx)
    (.write writer "</subroutineDec>\n")))

(defn compile-parameter-list [ctx]
  (let [writer (:writer @ctx)]
    ;; רשימת פרמטרים נפתחת ונסגרת תמיד, גם אם היא ריקה לפי דרישות ה-XML של התרגיל
    (.write writer "<parameterList>\n")
    (when-not (= ")" (peek-token ctx))
      (write-terminal! ctx) ;; type
      (write-terminal! ctx) ;; varName
      (while (= "," (peek-token ctx))
        (write-terminal! ctx) ;; ','
        (write-terminal! ctx) ;; type
        (write-terminal! ctx))) ;; varName
    (.write writer "</parameterList>\n")))

(defn compile-subroutine-body [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<subroutineBody>\n")
    (write-terminal! ctx) ;; '{'
    (while (= "var" (peek-token ctx))
      (compile-var-dec ctx))
    (compile-statements ctx)
    (write-terminal! ctx) ;; '}'
    (.write writer "</subroutineBody>\n")))

(defn compile-var-dec [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<varDec>\n")
    (write-terminal! ctx) ;; 'var'
    (write-terminal! ctx) ;; type
    (write-terminal! ctx) ;; varName
    (while (= "," (peek-token ctx))
      (write-terminal! ctx) ;; ','
      (write-terminal! ctx)) ;; varName
    (write-terminal! ctx) ;; ';'
    (.write writer "</varDec>\n")))

(defn compile-statements [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<statements>\n")
    (while (#{"let" "if" "while" "do" "return"} (peek-token ctx))
      (case (peek-token ctx)
        "let"    (compile-let ctx)
        "if"     (compile-if ctx)
        "while"  (compile-while ctx)
        "do"     (compile-do ctx)
        "return" (compile-return ctx)))
    (.write writer "</statements>\n")))

(defn compile-let [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<letStatement>\n")
    (write-terminal! ctx) ;; 'let'
    (write-terminal! ctx) ;; varName
    (when (= "[" (peek-token ctx))
      (write-terminal! ctx) ;; '['
      (compile-expression ctx)
      (write-terminal! ctx)) ;; ']'
    (write-terminal! ctx) ;; '='
    (compile-expression ctx)
    (write-terminal! ctx) ;; ';'
    (.write writer "</letStatement>\n")))

(defn compile-if [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<ifStatement>\n")
    (write-terminal! ctx) ;; 'if'
    (write-terminal! ctx) ;; '('
    (compile-expression ctx)
    (write-terminal! ctx) ;; ')'
    (write-terminal! ctx) ;; '{'
    (compile-statements ctx)
    (write-terminal! ctx) ;; '}'
    (when (= "else" (peek-token ctx))
      (write-terminal! ctx) ;; 'else'
      (write-terminal! ctx) ;; '{'
      (compile-statements ctx)
      (write-terminal! ctx)) ;; '}'
    (.write writer "</ifStatement>\n")))

(defn compile-while [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<whileStatement>\n")
    (write-terminal! ctx) ;; 'while'
    (write-terminal! ctx) ;; '('
    (compile-expression ctx)
    (write-terminal! ctx) ;; ')'
    (write-terminal! ctx) ;; '{'
    (compile-statements ctx)
    (write-terminal! ctx) ;; '}'
    (.write writer "</whileStatement>\n")))

(defn compile-do [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<doStatement>\n")
    (write-terminal! ctx) ;; 'do'
    (write-terminal! ctx) ;; subroutineName | className | varName
    (cond
      (= "(" (peek-token ctx))
      (do
        (write-terminal! ctx) ;; '('
        (compile-expression-list ctx)
        (write-terminal! ctx)) ;; ')'

      (= "." (peek-token ctx))
      (do
        (write-terminal! ctx) ;; '.'
        (write-terminal! ctx) ;; subroutineName
        (write-terminal! ctx) ;; '('
        (compile-expression-list ctx)
        (write-terminal! ctx))) ;; ')'
    (write-terminal! ctx) ;; ';'
    (.write writer "</doStatement>\n")))

(defn compile-return [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<returnStatement>\n")
    (write-terminal! ctx) ;; 'return'
    (when-not (= ";" (peek-token ctx))
      (compile-expression ctx))
    (write-terminal! ctx) ;; ';'
    (.write writer "</returnStatement>\n")))

(defn compile-expression [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<expression>\n")
    (compile-term ctx)
    (while (and (peek-token ctx)
                (= 1 (count (peek-token ctx)))
                (op-symbols (first (peek-token ctx))))
      (write-terminal! ctx) ;; op
      (compile-term ctx))   ;; term
    (.write writer "</expression>\n")))

(defn compile-term [ctx]
  (let [writer (:writer @ctx)
        next-t (peek-token ctx)]
    (.write writer "<term>\n")
    (cond
      ;; אופרטורים אונאריים: '-' או '~'
      (#{"-" "~"} next-t)
      (do
        (write-terminal! ctx) ;; unaryOp
        (compile-term ctx))

      ;; ביטוי עטוף בסוגריים עגולים
      (= "(" next-t)
      (do
        (write-terminal! ctx) ;; '('
        (compile-expression ctx)
        (write-terminal! ctx)) ;; ')'

      :else
      (let [t (peek-token ctx) ;; משתמשים ב-peek כדי לא לשלוף את הטוקן מהתור מוקדם מדי
            [tag value] (classify-token t)
            lookahead (let [remain (:tokens @ctx)]
                        (if (> (count remain) 1) (second remain) nil))] ;; מציצים לטוקן השני מבלי לגעת בתור
        (cond
          ;; גישה למערך בעזרת סוגריים מרובעים
          (= "[" lookahead)
          (do
            (write-terminal! ctx) ;; כותב את המזהה של המערך
            (write-terminal! ctx) ;; כותב את ה-'['
            (compile-expression ctx)
            (write-terminal! ctx)) ;; כותב את ה-']'

          ;; קריאה למתודה / פונקציה (למשל Screen.setColor או draw)
          (#{"(" "."} lookahead)
          (do
            (write-terminal! ctx) ;; כותב את המזהה הראשון (subroutineName / className / varName)
            (if (= "(" lookahead)
              (do
                (write-terminal! ctx) ;; כותב את ה-'('
                (compile-expression-list ctx)
                (write-terminal! ctx)) ;; כותב את ה-')'
              (do
                (write-terminal! ctx) ;; כותב את ה-'.'
                (write-terminal! ctx) ;; כותב את ה-subroutineName
                (write-terminal! ctx) ;; כותב את ה-'('
                (compile-expression-list ctx)
                (write-terminal! ctx)))) ;; כותב את ה-')'

          ;; טוקן בודד פשוט (מספר, מחרוזת נקייה, מזהה או קבוע)
          :else
          (write-terminal! ctx)))) ;; פשוט כותב את הטוקן ומתקדם בצורה בטוחה
    (.write writer "</term>\n")))

(defn compile-expression-list [ctx]
  (let [writer (:writer @ctx)]
    (.write writer "<expressionList>\n")
    (when-not (= ")" (peek-token ctx))
      (compile-expression ctx)
      (while (= "," (peek-token ctx))
        (write-terminal! ctx) ;; ','
        (compile-expression ctx)))
    (.write writer "</expressionList>\n")))


;; --- הפונקציה הראשית המקורית שלכן (בתוספת קריאה ל-Parser במקום ל-write-token-xml בלבד) ---
;; Main function that runs when the program is executed
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

                  ;; --- עדכון קל לשם קובץ הפלט לחלק ב' ---
                  ;; חלק א' ייצר קובץ בשם MYT.xml. חלק ב' (הנוכחי) אמור לייצר ישירות קובץ במבנה ההיררכי של הפרויקט עם סיומת PH.xml
                  output-path (str (str/replace (.getAbsolutePath f) #"\.jack$" "") "PH.xml")]

              ;; Open file for writing and perform output
              (with-open [writer (io/writer output-path)]
                ;; יצירת הקונטקסט והפעלת ה-Parser מהשורש (compile-class)
                (let [ctx (create-context tokens writer)]
                  (compile-class ctx)))

              ;; Print success message to the user
              (println "Created Structure XML:" (.getName (io/file output-path)))))))
      ;; Error message if the path was not found
      (println "Error: Directory not found. Checked path: [" path "]"))))