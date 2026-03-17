;;By: Tal Shezifi 213878580,Avital Hazan 214086092
;;Practice group number 150060.21.5786.42
;; Defines the namespace name. It must match the file path (src/tar0/core.clj).
(ns tar0.core
  ;; Imports Java I/O utilities for file handling, aliased as 'io'.
  (:require [clojure.java.io :as io]
    ;; Imports string manipulation functions, aliased as 'str'.
            [clojure.string :as str])
  ;; Tells Clojure to generate a Java class for this file, allowing it to run as a standalone application.
  (:gen-class))

;; Global atom to store the current file name without extension
(def current-file-name (atom ""))

;; --- Helper functions for Arithmetic commands ---

(defn handleAdd [writer]
  ;; Writes the 'add' command string to the output file
  (.write writer "command: add\n"))

(defn handleSub [writer]
  ;; Writes the 'sub' command string to the output file
  (.write writer "command: sub\n"))

(defn handleNeg [writer]
  ;; Writes the 'neg' command string to the output file
  (.write writer "command: neg\n"))

;; --- Helper functions for Logical commands ---

(defn handleEq [writer counter-atom]
  ;; Increments the logical counter and writes 'eq' with the current count
  (swap! counter-atom inc)
  (.write writer "command: eq\n")
  (.write writer (str "counter: " @counter-atom "\n")))

(defn handleGt [writer counter-atom]
  ;; Increments the logical counter and writes 'gt' with the current count
  (swap! counter-atom inc)
  (.write writer "command: gt\n")
  (.write writer (str "counter: " @counter-atom "\n")))

(defn handleLt [writer counter-atom]
  ;; Increments the logical counter and writes 'lt' with the current count
  (swap! counter-atom inc)
  (.write writer "command: lt\n")
  (.write writer (str "counter: " @counter-atom "\n")))

(defn handlePush [writer segment index]
  ;; Receives segment and index as parameters and writes formatted push command
  (.write writer (str "command: push segment " segment " index " index "\n")))

(defn handlePop [writer segment index]
  ;; Receives segment and index as parameters and writes formatted pop command
  (.write writer (str "command: pop segment " segment " index " index "\n")))

;; --- File and folder processing logic ---

(defn process-line [line writer logical-counter]
  ;; Split the line into words: trim whitespace and split by any whitespace character
  (let [words (str/split (str/trim line) #"\s+")]
    ;; Safety check: only process if the line is not empty and not just whitespace
    (when (and (seq line) (not (str/blank? line)))
      ;; 'case' acts as a router: checks the first word and decides which function to call
      (case (first words)
        ;; Arithmetic commands: only require the writer to output to the file
        "add"  (handleAdd writer)
        "sub"  (handleSub writer)
        "neg"  (handleNeg writer)

        ;; Comparison commands: also receive the logical-counter to create unique labels
        "eq"   (handleEq writer logical-counter)
        "gt"   (handleGt writer logical-counter)
        "lt"   (handleLt writer logical-counter)

        ;; Memory commands: send the second word (segment) and third word (index) as parameters
        "push" (handlePush writer (nth words 1) (nth words 2))
        "pop"  (handlePop writer (nth words 1) (nth words 2))

        ;; If the command is unknown or it's a comment, do nothing
        nil))))

(defn process-vm-file [file output-writer]
  ;; Extract the filename and remove the ".vm" extension for labeling and static variables
  (let [file-name (.getName file)
        base-name (str/replace file-name #"\.vm$" "")
        ;; Initialize a unique counter atom for logical commands (eq, gt, lt) for this specific file
        logical-counter (atom 0)]

    ;; Update the global current-file-name to ensure correct static variable mapping
    (reset! current-file-name base-name)

    ;; Open the input file for reading (ensures the resource is closed automatically)
    (with-open [rdr (io/reader file)]
      ;; Iterate over each line in the file sequence
      (doseq [line (line-seq rdr)]
        ;; Pass the line, the shared writer, and the local counter to the translation logic
        (process-line line output-writer logical-counter)))

    ;; Log the progress to the console once the file processing is finished
    (println (str "End of input file: " file-name))))

(defn -main [& args]
  ;; Prompt the user to enter the path of the directory containing .vm files
  (println "Please enter the directory path:")
  (flush) ;; Ensure the prompt is displayed immediately

  (let [path (read-line)
        directory (io/file path)]

    ;; Validate that the provided path exists and is actually a directory
    (if (and (.exists directory) (.isDirectory directory))
      (let [dir-name (.getName directory)
            ;; Define the output .asm filename based on the directory name
            output-file-name (str dir-name ".asm")
            ;; Create a file object for the output file within the same directory
            output-file (io/file directory output-file-name)

            ;; Filter the directory content: keep only files ending with ".vm"
            vm-files (filter #(and (.isFile %) (.endsWith (.getName %) ".vm"))
                             (.listFiles directory))]

        ;; Open a single output writer for the entire project
        (with-open [writer (io/writer output-file)]
          ;; Iterate through each identified VM file
          (doseq [f vm-files]
            ;; Process each file and write its translation into the shared output file
            (process-vm-file f writer)))

        ;; Notify the user that the translation process completed successfully
        (println (str "Output file is ready: " output-file-name)))

      ;; Handle cases where the directory path is invalid or missing
      (println "Error: Directory not found."))))

;;; "C:\Program Files\Java\jdk-22\bin\java.exe" ...
;;; Please enter the directory path:
;;; C:\Users\Dan\Documents\HST
;;; End of input file: InputA.vm
;;; End of input file: InputB.vm
;;; Output file is ready: HST.asm
;;; Process finished with exit code 0