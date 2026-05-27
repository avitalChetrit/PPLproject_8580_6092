;;By: Tal Shezifi 213878580, Avital Hazan 214086092
;;Practice group number 150060.21.5786.42
(ns tar0.stage5
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(declare classify-token)
;; --- קדם-הצהרות על פונקציות מחולל הקוד (VM Code Generator) ---
(declare compile-class compile-class-var-dec compile-subroutine
         compile-parameter-list compile-subroutine-body compile-var-dec
         compile-statements compile-let compile-if compile-while compile-do
         compile-return compile-expression compile-term compile-expression-list)

;; --- הגדרות שפת Jack ---
(def keywords #{"class" "constructor" "function" "method" "field" "static" "var"
                "int" "char" "boolean" "void" "true" "false" "null" "this"
                "let" "do" "if" "else" "while" "return"})

(def symbols #{\{ \} \( \) \[ \] \. \, \; \+ \- \* \/ \& \| \< \> \= \~})
(def op-symbols #{\+ \- \* \/ \& \| \< \> \=})

;; --- ניתוח טוקנים (Tokenizing) ---
(defn tokenize [input]
  (let [clean-code (-> input
                       (str/replace #"(?s)/\*.*?\*/" " ")
                       (str/replace #"//.*" " "))
        token-pattern #"\"[^\"]*\"|[a-zA-Z_][a-zA-Z0-9_]*|[0-9]+|[\{\}\(\)\[\]\.\,\;\+\-\*\/\&\|\<\>\=\~]"]
    (re-seq token-pattern clean-code)))

(defn classify-token [t]
  (cond
    (keywords t) [:keyword t]
    (re-matches #"[0-9]+" t) [:integerConstant t]
    (str/starts-with? t "\"") [:stringConstant (subs t 1 (dec (count t)))]
    (and (= 1 (count t)) (symbols (first t))) [:symbol t]
    :else [:identifier t]))

;; =================================================================================
;; --- תשתית ניהול המצב: טבלת סמלים ומחולל קוד VM (Symbol Table & VM Writer) ---
;; =================================================================================

(defn create-context [tokens writer]
  (atom {:tokens tokens
         :writer writer
         :class-name nil
         :class-table {}
         :subroutine-table {}
         :indices {"static" 0 "field" 0 "argument" 0 "local" 0}
         :label-counts {"if" 0 "while" 0}}))

(defn peek-token [ctx]
  (first (:tokens @ctx)))

(defn next-token! [ctx]
  (let [t (peek-token ctx)]
    (swap! ctx update :tokens rest)
    t))

(defn reset-subroutine-table! [ctx]
  (swap! ctx assoc :subroutine-table {})
  (swap! ctx update :indices assoc "argument" 0 "local" 0))

(defn add-symbol! [ctx name type kind]
  (let [current-idx (get-in @ctx [:indices kind])
        entry {:type type :kind kind :index current-idx}
        table-key (if (#{"static" "field"} kind) :class-table :subroutine-table)]
    (swap! ctx assoc-in [table-key name] entry)
    (swap! ctx update-in [:indices kind] inc)))

(defn lookup-symbol [ctx name]
  (or (get-in @ctx [:subroutine-table name])
      (get-in @ctx [:class-table name])))

(defn gen-label! [ctx type-prefix]
  (let [current-count (get-in @ctx [:label-counts type-prefix])]
    (swap! ctx update-in [:label-counts type-prefix] inc)
    (str (str/upper-case type-prefix) "_" current-count)))

(defn write-vm! [ctx line]
  (.write (:writer @ctx) (str line "\n")))

(defn write-push! [ctx segment index]
  (let [seg (if (= segment "field") "this" segment)]
    (write-vm! ctx (str "push " seg " " index))))

(defn write-pop! [ctx segment index]
  (let [seg (if (= segment "field") "this" segment)]
    (write-vm! ctx (str "pop " seg " " index))))

(defn write-arithmetic! [ctx command]
  (write-vm! ctx command))

;; =================================================================================
;; --- פונקציות הניתוח ומחולל הקוד (Compilation Engine) ---
;; =================================================================================

(defn compile-class [ctx]
  (next-token! ctx) ;; 'class'
  (let [c-name (next-token! ctx)]
    (swap! ctx assoc :class-name c-name))
  (next-token! ctx) ;; '{'

  (while (#{"static" "field"} (peek-token ctx))
    (compile-class-var-dec ctx))

  (while (#{"constructor" "function" "method"} (peek-token ctx))
    (compile-subroutine ctx))

  (next-token! ctx)) ;; '}'

(defn compile-class-var-dec [ctx]
  (let [kind (next-token! ctx)
        type (next-token! ctx)
        name (next-token! ctx)]
    (add-symbol! ctx name type kind)
    (while (= "," (peek-token ctx))
      (next-token! ctx)
      (add-symbol! ctx (next-token! ctx) type kind))
    (next-token! ctx))) ;; ';'

(defn compile-subroutine [ctx]
  (reset-subroutine-table! ctx)
  (let [subroutine-kind (next-token! ctx)
        return-type (next-token! ctx)
        subroutine-name (next-token! ctx)]

    (when (= subroutine-kind "method")
      (add-symbol! ctx "this" (:class-name @ctx) "argument"))

    (next-token! ctx) ;; '('
    (compile-parameter-list ctx)
    (next-token! ctx) ;; ')'

    (next-token! ctx) ;; '{'
    (while (= "var" (peek-token ctx))
      (compile-var-dec ctx))

    (let [num-locals (get-in @ctx [:indices "local"])
          full-subroutine-name (str (:class-name @ctx) "." subroutine-name)]
      (write-vm! ctx (str "function " full-subroutine-name " " num-locals)))

    (cond
      (= subroutine-kind "constructor")
      (let [num-fields (get-in @ctx [:indices "field"])]
        (write-push! ctx "constant" num-fields)
        (write-vm! ctx "call Memory.alloc 1")
        (write-pop! ctx "pointer" 0))

      (= subroutine-kind "method")
      (do
        (write-push! ctx "argument" 0)
        (write-pop! ctx "pointer" 0)))

    (compile-statements ctx)
    (next-token! ctx))) ;; '}'

(defn compile-parameter-list [ctx]
  (when-not (= ")" (peek-token ctx))
    (let [type (next-token! ctx)
          name (next-token! ctx)]
      (add-symbol! ctx name type "argument")
      (while (= "," (peek-token ctx))
        (next-token! ctx)
        (let [next-type (next-token! ctx)
              next-name (next-token! ctx)]
          (add-symbol! ctx next-name next-type "argument"))))))

(defn compile-var-dec [ctx]
  (next-token! ctx) ;; 'var'
  (let [type (next-token! ctx)
        name (next-token! ctx)]
    (add-symbol! ctx name type "local")
    (while (= "," (peek-token ctx))
      (next-token! ctx)
      (add-symbol! ctx (next-token! ctx) type "local"))
    (next-token! ctx))) ;; ';'

(defn compile-statements [ctx]
  (while (#{"let" "if" "while" "do" "return"} (peek-token ctx))
    (case (peek-token ctx)
      "let"    (compile-let ctx)
      "if"     (compile-if ctx)
      "while"  (compile-while ctx)
      "do"     (compile-do ctx)
      "return" (compile-return ctx))))

(defn compile-let [ctx]
  (next-token! ctx) ;; 'let'
  (let [var-name (next-token! ctx)
        sym (lookup-symbol ctx var-name)
        is-array (= "[" (peek-token ctx))]

    (if is-array
      (do
        (write-push! ctx (:kind sym) (:index sym))
        (next-token! ctx) ;; '['
        (compile-expression ctx)
        (next-token! ctx) ;; ']'
        (write-arithmetic! ctx "add")

        (next-token! ctx) ;; '='
        (compile-expression ctx)

        (write-pop! ctx "temp" 0)
        (write-pop! ctx "pointer" 1)
        (write-push! ctx "temp" 0)
        (write-pop! ctx "that" 0))

      (do
        (next-token! ctx) ;; '='
        (compile-expression ctx)
        (write-pop! ctx (:kind sym) (:index sym))))
    (next-token! ctx))) ;; ';'

(defn compile-if [ctx]
  (let [label-true (gen-label! ctx "if")
        label-false (gen-label! ctx "if")
        label-end (gen-label! ctx "if")]
    (next-token! ctx) ;; 'if'
    (next-token! ctx) ;; '('
    (compile-expression ctx)
    (next-token! ctx) ;; ')'

    (write-vm! ctx (str "if-goto " label-true))
    (write-vm! ctx (str "goto " label-false))
    (write-vm! ctx (str "label " label-true))

    (next-token! ctx) ;; '{'
    (compile-statements ctx)
    (next-token! ctx) ;; '}'

    (if (= "else" (peek-token ctx))
      (do
        (write-vm! ctx (str "goto " label-end))
        (write-vm! ctx (str "label " label-false))
        (next-token! ctx) ;; 'else'
        (next-token! ctx) ;; '{'
        (compile-statements ctx)
        (next-token! ctx) ;; '}'
        (write-vm! ctx (str "label " label-end)))
      (write-vm! ctx (str "label " label-false)))))

(defn compile-while [ctx]
  (let [label-exp (gen-label! ctx "while")
        label-end (gen-label! ctx "if")]
    (write-vm! ctx (str "label " label-exp))
    (next-token! ctx) ;; 'while'
    (next-token! ctx) ;; '('
    (compile-expression ctx)
    (next-token! ctx) ;; ')'

    (write-arithmetic! ctx "not")
    (write-vm! ctx (str "if-goto " label-end))

    (next-token! ctx) ;; '{'
    (compile-statements ctx)
    (next-token! ctx) ;; '}'

    (write-vm! ctx (str "goto " label-exp))
    (write-vm! ctx (str "label " label-end))))

(defn compile-do [ctx]
  (next-token! ctx) ;; 'do'
  (let [first-id (next-token! ctx)]
    (cond
      (= "(" (peek-token ctx))
      (do
        (write-push! ctx "pointer" 0)
        (next-token! ctx) ;; '('
        (let [n-args (compile-expression-list ctx)]
          (next-token! ctx) ;; ')'
          (write-vm! ctx (str "call " (:class-name @ctx) "." first-id " " (inc n-args)))))

      (= "." (peek-token ctx))
      (do
        (next-token! ctx) ;; '.'
        (let [subroutine-name (next-token! ctx)
              sym (lookup-symbol ctx first-id)]
          (next-token! ctx) ;; '('
          (if sym
            (do
              (write-push! ctx (:kind sym) (:index sym))
              (let [n-args (compile-expression-list ctx)]
                (next-token! ctx) ;; ')'
                (write-vm! ctx (str "call " (:type sym) "." subroutine-name " " (inc n-args)))))
            (let [n-args (compile-expression-list ctx)]
              (next-token! ctx) ;; ')'
              (write-vm! ctx (str "call " first-id "." subroutine-name " " n-args))))))))

  (next-token! ctx) ;; ';'
  (write-pop! ctx "temp" 0))

(defn compile-return [ctx]
  (next-token! ctx) ;; 'return'
  (if (= ";" (peek-token ctx))
    (write-push! ctx "constant" 0)
    (compile-expression ctx))
  (next-token! ctx) ;; ';'
  (write-vm! ctx "return"))

(defn compile-expression [ctx]
  (compile-term ctx)
  (while (and (peek-token ctx)
              (= 1 (count (peek-token ctx)))
              (op-symbols (first (peek-token ctx))))
    (let [op (next-token! ctx)]
      (compile-term ctx)
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
        [token-tag token-val] (classify-token next-t)]
    (cond
      (#{"-" "~"} next-t)
      (do
        (next-token! ctx)
        (compile-term ctx)
        (if (= next-t "-")
          (write-arithmetic! ctx "neg")
          (write-arithmetic! ctx "not")))

      (= "(" next-t)
      (do
        (next-token! ctx) ;; בולע את '('
        (compile-expression ctx)
        (next-token! ctx)) ;; **תיקון: בולע כעת בצורה מוחלטת את ')'**

      (= token-tag :integerConstant)
      (write-push! ctx "constant" (next-token! ctx))

      (= token-tag :stringConstant)
      (let [s (next-token! ctx)
            clean-str (subs s 1 (dec (count s)))
            len (count clean-str)]
        (write-push! ctx "constant" len)
        (write-vm! ctx "call String.new 1")
        (doseq [ch clean-str]
          (write-push! ctx "constant" (int ch))
          (write-vm! ctx "call String.appendChar 2")))

      (#{"false" "null"} next-t)
      (do (next-token! ctx) (write-push! ctx "constant" 0))

      (= "true" next-t)
      (do (next-token! ctx) (write-push! ctx "constant" 0) (write-arithmetic! ctx "not"))

      (= "this" next-t)
      (do (next-token! ctx) (write-push! ctx "pointer" 0))

      :else
      (let [first-id (next-token! ctx)
            lookahead (peek-token ctx)] ;; **תיקון: מציץ על הטוקן הבא אחרי שבלענו את ה-id**
        (cond
          (= "[" lookahead)
          (let [sym (lookup-symbol ctx first-id)]
            (write-push! ctx (:kind sym) (:index sym))
            (next-token! ctx) ;; '['
            (compile-expression ctx)
            (next-token! ctx) ;; ']'
            (write-arithmetic! ctx "add")
            (write-pop! ctx "pointer" 1)
            (write-push! ctx "that" 0))

          (#{"(" "."} lookahead)
          (do
            ;; **תיקון זרם הבליעה לקריאות מובלעות בתוך ביטויים**
            (next-token! ctx) ;; בולע את ה-'.' או את ה-'('
            (if (= "(" lookahead)
              (do
                (write-push! ctx "pointer" 0)
                (let [n-args (compile-expression-list ctx)]
                  (next-token! ctx) ;; בולע ')'
                  (write-vm! ctx (str "call " (:class-name @ctx) "." first-id " " (inc n-args)))))
              (let [subroutine-name (next-token! ctx)]
                (next-token! ctx) ;; בולע את ה-'(' שמופיע אחרי השם
                (let [sym (lookup-symbol ctx first-id)]
                  (if sym
                    (do
                      (write-push! ctx (:kind sym) (:index sym))
                      (let [n-args (compile-expression-list ctx)]
                        (next-token! ctx) ;; בולע ')'
                        (write-vm! ctx (str "call " (:type sym) "." subroutine-name " " (inc n-args)))))
                    (let [n-args (compile-expression-list ctx)]
                      (next-token! ctx) ;; בולע ')'
                      (write-vm! ctx (str "call " first-id "." subroutine-name " " n-args))))))))

          :else
          (let [sym (lookup-symbol ctx first-id)]
            (write-push! ctx (:kind sym) (:index sym))))))))

(defn compile-expression-list [ctx]
  (let [arg-count (atom 0)]
    (when-not (= ")" (peek-token ctx))
      (compile-expression ctx)
      (swap! arg-count inc)
      (while (= "," (peek-token ctx))
        (next-token! ctx)
        (compile-expression ctx)
        (swap! arg-count inc)))
    @arg-count))

;; --- פונקציית הריצה הראשית (Main) ---
(defn -main [& args]
  (let [raw-path (or (first args)
                     (do (println "Please enter the directory path:")
                         (read-line)))
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
                  output-path (str (str/replace (.getAbsolutePath f) #"\.jack$" "") ".vm")]
              (with-open [writer (io/writer output-path)]
                (let [ctx (create-context tokens writer)]
                  (compile-class ctx)))
              (println "Created VM Code:" (.getName (io/file output-path)))))))
      (println "Error: Directory not found. Checked path: [" path "]"))))