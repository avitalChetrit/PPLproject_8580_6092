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
  (swap! counter inc);; Increment the counter to create unique labels for this instance
  (let [i @counter                     ;; Retrieve the current counter value
        label-true (str "EQ_TRUE" i);; Define a unique label for the "True" case
        label-false (str "EQ_FALSE" i)];; Define a unique label for the "False" case (end of command)
    (write-asm writer
               ["// eq"
                "@SP" "AM=M-1" "D=M"
                "A=A-1" "D=M-D"
                (str "@" label-true) "D;JEQ" ;;If D == 0 (meaning x == y), jump to the TRUE label
                "@SP" "A=M-1" "M=0";; If not equal (False): set the value at the top of the stack to 0
                (str "@" label-false) "0;JMP" ;; Jump to the end to skip the TRUE block
                (str "(" label-true ")")
                "@SP" "A=M-1" "M=-1"
                (str "(" label-false ")")])));; Definition of the FALSE label (End of operation)


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
                (str "@" label-true) "D;JGT";; If D > 0 (meaning x > y), jump to TRUE
                "@SP" "A=M-1" "M=0" ;; Result is False (0)
                (str "@" label-false) "0;JMP";; Jump to end
                (str "(" label-true ")")
                "@SP" "A=M-1" "M=-1";; Result is True (-1)(M=-1)
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
                (str "@" label-true) "D;JLT" ;; If D < 0 (meaning x < y), jump to TRUE
                "@SP" "A=M-1" "M=0" ;; Result is False (0)
                (str "@" label-false) "0;JMP";; Jump to end
                (str "(" label-true ")")
                "@SP" "A=M-1" "M=-1"
                (str "(" label-false ")")])));; Result is True (-1)=(M=-1)

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
    ;; --- Push constant value onto the stack ---
    "constant"
    (write-asm writer [(str "// push constant " index)
                       (str "@" index) "D=A";; Load the constant value into register D
                       "@SP" "A=M" "M=D"
                       "@SP" "M=M+1"])
    ;; --- Push from base-indexed segments (local, argument, this, that) ---
    ("local" "argument" "this" "that")
    (let [base (segment-map segment)]
      ;; The line above looks up the mapping (e.g., "local" -> "LCL")
      ;; to identify which base register to use for address calculation.
      (write-asm writer [(str "// push " segment " " index)
                         (str "@" index) "D=A";; Store the index (offset) in D
                         (str "@" base) "A=M+D";; Calculate target address: Base value + Offset
                         "D=M"
                         "@SP" "A=M" "M=D"
                         "@SP" "M=M+1"]))
    ;; --- Push from temporary segment (temp) - Fixed addresses 5 to 12 ---
    "temp"
    (write-asm writer [(str "// push temp " index)
                       ;; Calculate absolute address: 5 + index, and fetch value to D
                       (str "@" (+ 5 (Integer/parseInt index)))
                       "D=M"
                       "@SP" "A=M" "M=D"
                       "@SP" "M=M+1"])

    "pointer"
    ;; --- Push from pointer segment (pointer) - Directly accesses THIS or THAT ---
    (let [target (if (= index "0") "THIS" "THAT")]
      ;; This line checks the index: if it's 0, the target is the THIS register;
      ;; if it's 1, the target is the THAT register.
      (write-asm writer [(str "// push pointer " index)
                         (str "@" target) "D=M"  ;; Fetch the base address stored in THIS/THAT into D
                         "@SP" "A=M" "M=D"
                         "@SP" "M=M+1"]))

    "static"
    ;; --- Push from static segment (static) - Using global labels ---
    (write-asm writer [(str "// push static " index)
                       ;; Accesses a variable whose name consists of the file name and index
                       (str "@" @current-file-name "." index)
                       "D=M"
                       "@SP" "A=M" "M=D"
                       "@SP" "M=M+1"])))

;; pop command implementation
(defn handlePop [writer segment index]
  (case segment
    ;; --- Pop from base-indexed segments (local, argument, this, that) ---
    ("local" "argument" "this" "that")
    (let [base (segment-map segment)]
      ;; The line above looks up the mapping (e.g., "local" -> "LCL")
      ;; to identify which base register to use for address calculation.
      (write-asm writer [(str "// pop " segment " " index)
                         (str "@" index) "D=A"
                         (str "@" base) "D=M+D"
                         "@R13" "M=D";; Temporarily store the target address in R13
                         "@SP" "AM=M-1" "D=M"
                         "@R13" "A=M" "M=D"]))
    ;; --- Pop into temporary segment (temp) - Fixed addresses 5 to 12 ---
    "temp"
    (write-asm writer [(str "// pop temp " index)
                       "@SP" "AM=M-1" "D=M"
                       ;; Write the value directly to the fixed memory address (5 + index)
                       (str "@" (+ 5 (Integer/parseInt index)))
                       "M=D"])

    ;; --- Pop into pointer segment (pointer) - Directly updates THIS or THAT ---
    "pointer"
    (let [target (if (= index "0") "THIS" "THAT")]
      ;; This line checks the index: if it's 0, the target is the THIS register;
      ;; if it's 1, the target is the THAT register.
      (write-asm writer [(str "// pop pointer " index)
                         "@SP" "AM=M-1" "D=M"
                         (str "@" target) "M=D"]));; Update the THIS or THAT register with the popped value

    ;; --- Pop into static segment (static) - Using global labels ---
    "static"
    (write-asm writer [(str "// pop static " index)
                       "@SP" "AM=M-1" "D=M"
                       ;; Write the value into the static variable label: [FileName].[Index]
                       (str "@" @current-file-name "." index)
                       "M=D"])))


;; --- Program Flow Commands ---
(defn handleLabel [writer label]
  (write-asm writer [(str "(" @current-file-name "$" label ")")]))

(defn handleGOTo [writer label]
  (write-asm writer [(str "@" @current-file-name "$" label) "0 ;JMP"]))

(defn handleIfGOTo [writer label]
  (write-asm writer [(str "// if-goto" "@SP" "A=M-1" "D=M" @current-file-name "$" label) "D;JNE"]))
;; --------------------------------------------------
;; Processing input lines
;; --------------------------------------------------

(defn process-line [line writer counter]
  ;; --- Clean the input line by removing leading and trailing whitespace ---
  (let [line (str/trim line)]
    ;; --- Ignore the line if it's empty or starts with a comment (//) ---
    (when (and (not (str/blank? line))
               (not (str/starts-with? line "//")))
      ;; --- Split the line into individual words based on whitespace ---
      (let [words (str/split line #"\s+")]
        ;; --- Identify the command type and call the corresponding handler ---
        (case (first words)
          ;; Arithmetic and logical commands
          "add" (handleAdd writer)
          "sub" (handleSub writer)
          "neg" (handleNeg writer)
          "and" (handleAnd writer)
          "or"  (handleOr writer)
          "not" (handleNot writer)

          ;; Comparison commands (require a unique counter for jump labels)
          "eq"  (handleEq writer counter)
          "gt"  (handleGt writer counter)
          "lt"  (handleLt writer counter)

          ;; Memory access commands: take segment name and index as arguments
          "push" (handlePush writer (nth words 1) (nth words 2))
          "pop"  (handlePop writer (nth words 1) (nth words 2))
          nil)))))

;; --- Process single VM file ---
(defn process-vm-file [file writer]
  (let [file-name (.getName file)
        ;; --- Strip the .vm extension to get the base name for static variables ---
        base-name (str/replace file-name #"\.vm$" "")
        ;; --- Initialize a local counter for this file to ensure unique labels ---
        counter (atom 0)]
    ;; --- Set the global file name context for 'static' segment handling ---
    (reset! current-file-name base-name)
    ;; --- Open the VM file for reading ---
    (with-open [r (io/reader file)]
      ;; --- Iterate through every line in the file and process it ---
      (doseq [line (line-seq r)]
        (process-line line writer counter)))
    (println (str "End of input file: " file-name))))

;; --------------------------------------------------
;; Main program
;; --------------------------------------------------

(defn -main [& args]
  ;; --- Prompt user for the directory containing .vm files ---
  (println "Please enter the directory path:")
  (flush)

  (let [path (read-line)
        dir (io/file path)]
    ;; --- Check if the provided path is a valid directory ---
    (if (and (.exists dir) (.isDirectory dir))
      (let [dir-name (.getName dir)
            ;; --- Create the output .asm file named after the directory ---
            output (io/file dir (str dir-name ".asm"))
            ;; --- Collect, filter for .vm files, and sort them alphabetically ---
            files (->> (.listFiles dir)
                       (filter #(and (.isFile %) (.endsWith (.getName %) ".vm")))
                       (sort-by #(.getName %)))]
        ;; --- Open the output file writer ---
        (with-open [writer (io/writer output)]
          ;; --- Process each VM file found in the directory sequence ---
          (doseq [f files]
            (process-vm-file f writer)))
        (println (str "Output file is ready: " (.getName output))))
      ;; --- Handle cases where the directory path is invalid ---
      (println "Error: Directory not found."))))