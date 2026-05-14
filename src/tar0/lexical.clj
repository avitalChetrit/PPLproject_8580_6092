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

;; --- Jack language settings ---

;; Define a set of all reserved keywords in the Jack language
(def keywords #{"class" "constructor" "function" "method" "field" "static" "var"
                "int" "char" "boolean" "void" "true" "false" "null" "this"
                "let" "do" "if" "else" "while" "return"})

;; Define a set of all allowed symbols in the Jack language
(def symbols #{\{ \} \( \) \[ \] \. \, \; \+ \- \* \/ \& \| \< \> \= \~})

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

;; --- זיהוי וסיווג טוקנים ---
(defn classify-token [t]
  (cond
    ;; Keyword
    (keywords t) [:keyword t]

    ;; Integer Constant
    (re-matches #"[0-9]+" t)
    (let [n (Integer/parseInt t)]
      (if (<= 0 n 32767)
        [:integerConstant t]
        [:identifier t])) ;; אם חרג מהטווח, Jack מתייחסת לזה לעיתים כמזהה או שגיאה
    ;; String Constant (הסרת המירכאות)
    (str/starts-with? t "\"")
    [:stringConstant (subs t 1 (dec (count t)))]

    ;; Symbol
    (and (= 1 (count t)) (symbols (first t)))
    [:symbol t] ;; כאן לא עושים escape-xml, הוא יבוצע בשלב הכתיבה
    ;; Identifier
    :else
    [:identifier t]))

;; --- פונקציה ראשית ---
(defn -main [& args]
  ;; אם הנתיב הועבר כארגומנט ב-lein run, נשתמש בו.
  ;; אם לא, נבקש מהמשתמש להקיש אותו.
  (let [raw-path (or (first args)
                     (do (println "Please enter the directory path:")
                         (read-line)))
        ;; שימוש ב-trim הוא קריטי כדי להוריד אנטרים או רווחים מיותרים
        path (str/trim (str raw-path))
        dir (io/file path)]

    (if (.exists dir)
      (let [files (if (.isDirectory dir)
                    (filter #(str/ends-with? (.getName %) ".jack") (.listFiles dir))
                    [dir])]
        (if (empty? files)
          (println "No .jack files found in the directory.")
          (doseq [f files]
            (let [file-content (slurp f)
                  tokens (tokenize file-content)
                  output-path (str (str/replace (.getAbsolutePath f) #"\.jack$" "") "MYT.xml")]
              (with-open [writer (io/writer output-path)]
                (write-token-xml tokens writer))
              (println "Created Token XML:" (.getName (io/file output-path)))))))
      ;; הדפסה שתעזור לך לראות אם הנתיב הגיע משובש
      (println "Error: Directory not found. Checked path: [" path "]"))))