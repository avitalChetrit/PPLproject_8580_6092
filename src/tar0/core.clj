;;By: Tal Shezifi 213878580, Avital Hazan 214086092
;;Practice group number 150060.21.5786.42

(ns tar0.core
  ;; Imports Java I/O utilities for file handling
  (:require [clojure.java.io :as io]
    ;; Imports string manipulation functions
            [clojure.string :as str])
  ;; Allows running as standalone application
  (:gen-class))

;; Global atom to store current file name (for static segment)
(def current-file-name (atom ""))

;; --------------------------------------------------
;; Helper function to write multiple assembly lines
;; --------------------------------------------------
(defn write-asm [writer lines]
  (doseq [line lines]
    (.write writer (str line "\n"))))

;; --------------------------------------------------
;; Arithmetic commands
;; --------------------------------------------------

;; add: pop y, pop x → push (x+y)
(defn handleAdd [writer]
  (write-asm writer
             ["// add"
              "@SP" "A=M-1" "D=M"
              "A=A-1" "M=D+M"
              "@SP" "M=M-1"]))

;; sub: pop y, pop x → push (x-y)
(defn handleSub [writer]
  (write-asm writer
             ["// sub"
              "@SP" "A=M-1" "D=M"
              "A=A-1" "M=M-D"
              "@SP" "M=M-1"]))

;; neg: negate top of stack
(defn handleNeg [writer]
  (write-asm writer
             ["// neg"
              "@SP" "A=M-1" "M=-M"]))

;; and: bitwise and
(defn handleAnd [writer]
  (write-asm writer
             ["// and"
              "@SP" "A=M-1" "D=M"
              "A=A-1" "M=D&M"
              "@SP" "M=M-1"]))

;; or: bitwise or
(defn handleOr [writer]
  (write-asm writer
             ["// or"
              "@SP" "A=M-1" "D=M"
              "A=A-1" "M=D|M"
              "@SP" "M=M-1"]))

;; not: bitwise not
(defn handleNot [writer]
  (write-asm writer
             ["// not"
              "@SP" "A=M-1" "M=!M"]))

;; --------------------------------------------------
;; Logical commands (with unique labels)
;; --------------------------------------------------

;; eq: push true(-1) if equal else false(0)
(defn handleEq [writer counter]
  (swap! counter inc)
  (let [i @counter
        label-true (str "EQ_TRUE" i)
        label-false (str "EQ_FALSE" i)]
    (write-asm writer
               ["// eq"
                "@SP" "AM=M-1" "D=M"
                "A=A-1" "D=M-D"
                (str "@" label-true) "D;JEQ"
                "@SP" "A=M-1" "M=0"
                (str "@" label-false) "0;JMP"
                (str "(" label-true ")")
                "@SP" "A=M-1" "M=-1"
                (str "(" label-false ")")])))

;; gt: push true if x > y
(defn handleGt [writer counter]
  (swap! counter inc)
  (let [i @counter
        label-true (str "GT_TRUE" i)
        label-false (str "GT_FALSE" i)]
    (write-asm writer
               ["// gt"
                "@SP" "AM=M-1" "D=M"
                "A=A-1" "D=M-D"
                (str "@" label-true) "D;JGT"
                "@SP" "A=M-1" "M=0"
                (str "@" label-false) "0;JMP"
                (str "(" label-true ")")
                "@SP" "A=M-1" "M=-1"
                (str "(" label-false ")")])))

;; lt: push true if x < y
(defn handleLt [writer counter]
  (swap! counter inc)
  (let [i @counter
        label-true (str "LT_TRUE" i)
        label-false (str "LT_FALSE" i)]
    (write-asm writer
               ["// lt"
                "@SP" "AM=M-1" "D=M"
                "A=A-1" "D=M-D"
                (str "@" label-true) "D;JLT"
                "@SP" "A=M-1" "M=0"
                (str "@" label-false) "0;JMP"
                (str "(" label-true ")")
                "@SP" "A=M-1" "M=-1"
                (str "(" label-false ")")])))

;; --------------------------------------------------
;; Memory segments
;; --------------------------------------------------

;; Maps VM segments to Hack assembly symbols
(def segment-map
  {"local" "LCL"
   "argument" "ARG"
   "this" "THIS"
   "that" "THAT"})

;; push command implementation
(defn handlePush [writer segment index]
  (case segment
    "constant"
    (write-asm writer [(str "// push constant " index)
                       (str "@" index) "D=A"
                       "@SP" "A=M" "M=D"
                       "@SP" "M=M+1"])

    ("local" "argument" "this" "that")
    (let [base (segment-map segment)]
      (write-asm writer [(str "// push " segment " " index)
                         (str "@" index) "D=A"
                         (str "@" base) "A=M+D"
                         "D=M"
                         "@SP" "A=M" "M=D"
                         "@SP" "M=M+1"]))

    "temp"
    (write-asm writer [(str "// push temp " index)
                       (str "@" (+ 5 (Integer/parseInt index)))
                       "D=M"
                       "@SP" "A=M" "M=D"
                       "@SP" "M=M+1"])

    "pointer"
    (let [target (if (= index "0") "THIS" "THAT")]
      (write-asm writer [(str "// push pointer " index)
                         (str "@" target) "D=M"
                         "@SP" "A=M" "M=D"
                         "@SP" "M=M+1"]))

    "static"
    (write-asm writer [(str "// push static " index)
                       (str "@" @current-file-name "." index)
                       "D=M"
                       "@SP" "A=M" "M=D"
                       "@SP" "M=M+1"])))

;; pop command implementation
(defn handlePop [writer segment index]
  (case segment
    ("local" "argument" "this" "that")
    (let [base (segment-map segment)]
      (write-asm writer [(str "// pop " segment " " index)
                         (str "@" index) "D=A"
                         (str "@" base) "D=M+D"
                         "@R13" "M=D"
                         "@SP" "AM=M-1" "D=M"
                         "@R13" "A=M" "M=D"]))

    "temp"
    (write-asm writer [(str "// pop temp " index)
                       "@SP" "AM=M-1" "D=M"
                       (str "@" (+ 5 (Integer/parseInt index)))
                       "M=D"])

    "pointer"
    (let [target (if (= index "0") "THIS" "THAT")]
      (write-asm writer [(str "// pop pointer " index)
                         "@SP" "AM=M-1" "D=M"
                         (str "@" target) "M=D"]))

    "static"
    (write-asm writer [(str "// pop static " index)
                       "@SP" "AM=M-1" "D=M"
                       (str "@" @current-file-name "." index)
                       "M=D"])))

;; --------------------------------------------------
;; Processing input lines
;; --------------------------------------------------

(defn process-line [line writer counter]
  (let [line (str/trim line)]
    ;; Ignore empty lines and comments
    (when (and (not (str/blank? line))
               (not (str/starts-with? line "//")))
      (let [words (str/split line #"\s+")]
        (case (first words)
          "add" (handleAdd writer)
          "sub" (handleSub writer)
          "neg" (handleNeg writer)
          "and" (handleAnd writer)
          "or"  (handleOr writer)
          "not" (handleNot writer)
          "eq"  (handleEq writer counter)
          "gt"  (handleGt writer counter)
          "lt"  (handleLt writer counter)
          "push" (handlePush writer (nth words 1) (nth words 2))
          "pop"  (handlePop writer (nth words 1) (nth words 2))
          nil)))))

;; Process single VM file
(defn process-vm-file [file writer]
  (let [file-name (.getName file)
        base-name (str/replace file-name #"\.vm$" "")
        counter (atom 0)]
    (reset! current-file-name base-name)
    (with-open [r (io/reader file)]
      (doseq [line (line-seq r)]
        (process-line line writer counter)))
    (println (str "End of input file: " file-name))))

;; --------------------------------------------------
;; Main program
;; --------------------------------------------------

(defn -main [& args]
  (println "Please enter the directory path:")
  (flush)

  (let [path (read-line)
        dir (io/file path)]
    (if (and (.exists dir) (.isDirectory dir))
      (let [dir-name (.getName dir)
            output (io/file dir (str dir-name ".asm"))
            files (->> (.listFiles dir)
                       (filter #(and (.isFile %) (.endsWith (.getName %) ".vm")))
                       (sort-by #(.getName %)))]
        (with-open [writer (io/writer output)]
          (doseq [f files]
            (process-vm-file f writer)))
        (println (str "Output file is ready: " (.getName output))))
      (println "Error: Directory not found."))))