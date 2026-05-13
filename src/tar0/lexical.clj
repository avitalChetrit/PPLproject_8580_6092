(ns jack.lexical
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

;; --- הגדרות שפת Jack ---
(def keywords #{"class" "constructor" "function" "method" "field" "static" "var"
                "int" "char" "boolean" "void" "true" "false" "null" "this"
                "let" "do" "if" "else" "while" "return"})

(def symbols #{\{ \} \( \) \[ \] \. \, \; \+ \- \* \/ \& \| \< \> \= \~})

;; --- טיפול בתווים מיוחדים עבור XML ---
(defn escape-xml [s]
  (case s
    "<" "&lt;"
    ">" "&gt;"
    "\"" "&quot;"
    "&" "&amp;"
    s))

;; --- זיהוי טוקנים (Tokenizer) ---
(defn classify-token [t]
  (cond
    ;; בדיקה אם המילה היא מילה שמורה (Keyword)
    (keywords t)
    [:keyword t]

    ;; בדיקה אם הטוקן הוא מספר (Integer Constant) בטווח התקין
    (re-matches #"[0-9]+" t)
    (let [n (Integer/parseInt t)]
      (if (<= 0 n 32767)
        [:integerConstant t]
        ;; אם המספר מחוץ לטווח, הוא לא נחשב ל-IntegerConstant לפי דקדוק השפה
        ))

    ;; בדיקה אם הטוקן הוא מחרוזת (String Constant) - הסרת הגרשיים
    (str/starts-with? t "\"")
    [:stringConstant (str/replace t "\"" "")]

    ;; בדיקה אם הטוקן הוא סימבול (Symbol) - טיפול בתווי XML מיוחדים
    (symbols (first t))
    [:symbol (escape-xml t)]

    ;; בכל מקרה אחר, זהו מזהה (Identifier) - שמות משתנים, פונקציות וכו'
    :else
    [:identifier t]))

;; --- כתיבת קובץ XML (חלק א') ---
(defn write-token-xml [tokens writer]
  (.write writer "<tokens>\n")
  (doseq [t tokens]
    (let [[tag value] (classify-token t)]
      (.write writer (format "<%s> %s </%s>\n" (name tag) value (name tag)))))
  (.write writer "</tokens>\n"))

;; --- פונקציה ראשית ---
(defn -main [& args]
  (let [path (first args)
        dir (io/file path)]
    (if (and (.exists dir) (.isDirectory dir))
      (let [files (filter #(and (.isFile %) (.endsWith (.getName %) ".jack"))
                          (.listFiles dir))]
        (doseq [f files]
          (let [file-content (slurp f)
                tokens (tokenize file-content)
                output-name (str/replace (.getAbsolutePath f) #"\.jack$" "T.xml")
                output-file (io/file output-name)]
            (with-open [writer (io/writer output-file)]
              (write-token-xml tokens writer))
            (println "Created Token XML:" (.getName output-file)))))
      (println "Error: Invalid directory path."))))