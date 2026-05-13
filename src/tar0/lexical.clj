(ns tar0.lexical
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))
(declare classify-token)
;; --- הגדרות שפת Jack ---
(def keywords #{"class" "constructor" "function" "method" "field" "static" "var"
                "int" "char" "boolean" "void" "true" "false" "null" "this"
                "let" "do" "if" "else" "while" "return"})

(def symbols #{\{ \} \( \) \[ \] \. \, \; \+ \- \* \/ \& \| \< \> \= \~})

;; --- טיפול בתווים מיוחדים עבור XML ---
(defn escape-xml [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

;; --- פירוק לטוקנים (Tokenizing) ---
(defn tokenize [input]
  (let [
        clean-code (-> input
                       (str/replace #"(?s)/\*.*?\*/" " ")
                       (str/replace #"//.*" " "))
        ;; ה-Regex הזה מפריד מחרוזות, אז מילים/מספרים, ואז סימנים
        token-pattern #"\"[^\"]*\"|[a-zA-Z_][a-zA-Z0-9_]*|[0-9]+|[\{\}\(\)\[\]\.\,\;\+\-\*\/\&\|\<\>\=\~]"]
(re-seq token-pattern clean-code)))

(defn write-token-xml [tokens writer]
  (.write writer "<tokens>\n")
  (doseq [t tokens]
    (let [[tag value] (classify-token t)
          ;; וודא שכל ערך עובר escape_xml כדי לא לשבור את ה-XML
          final-value (escape-xml value)]
      (.write writer (str "<" (name tag) "> " final-value " </" (name tag) ">\n"))))
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
                  output-path (str (str/replace (.getAbsolutePath f) #"\.jack$" "") "T.xml")]
              (with-open [writer (io/writer output-path)]
                (write-token-xml tokens writer))
              (println "Created Token XML:" (.getName (io/file output-path)))))))
      ;; הדפסה שתעזור לך לראות אם הנתיב הגיע משובש
      (println "Error: Directory not found. Checked path: [" path "]"))))