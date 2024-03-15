;;;; hack-assembler.lisp

(in-package #:hack-assembler)


(defclass label-resolution ()
  ((label-to-value :initform nil :accessor label-to-value))
  (:documentation "A mapping from label names to integer values."))

(defun stripped-line (stream)
  "Read a line from stream, skipping whitespace-only lines, trimming surrounding
whitespace, and removing comments"
  (i:iterate
    (i:for next-line next (read-line stream))
    (i:for stripped-line = (str:trim (cl-ppcre:regex-replace-all "//.*" next-line "")))
    (when (equal "" stripped-line)
      (i:next-iteration))
    (i:leave stripped-line)))

(defun add-label (label-resolution label value)
  "Add an integer value for a label. If value is nil, do not overwrite existing values. If multiple non-nil values passed, error"
  (let ((old-value (assoc label (label-to-value label-resolution) :test 'equal)))
    (if old-value
        (when value
          (assert (null (cdr old-value)) nil
                  (error
                   (format nil "Multiple values for label ~a!" label)))
          (setf (cdr old-value) value))
        (push `(,label . ,value) (label-to-value label-resolution)))))

(defun process-line-for-labels (label-resolution line line-num)
  "Add label information for a given line."
  (when (equal (aref line 0) #\@)
    (let ((label (str:substring 1 nil line)))
      (when (cl-ppcre:scan "[^0-9]" label)
        (add-label label-resolution label nil))))
  (trivia:match line
    ((trivia.ppcre:ppcre ("\\((.*)\\)") label) (add-label label-resolution label line-num))))

(defun assign-variables (label-resolution)
  "Give all referenced labels that have not already been given a value a value"
  (let ((num-variables (i:iterate
                         (i:for (nil . num) in (label-to-value label-resolution))
                         (i:counting (null num)))))
    (i:iterate
      (i:for (label . num) in (label-to-value label-resolution))
      (when (null num)
        (add-label label-resolution label (+ 15 num-variables))
        (decf num-variables)))))

(defun generate-label-resolutions (stream)
  (let ((ret (make-instance 'label-resolution)))
    ;; Add the virtual registers
    (add-label ret "SP" 0)
    (add-label ret "LCL" 1)
    (add-label ret "ARG" 2)
    (add-label ret "THIS" 3)
    (add-label ret "THAT" 4)
    (i:iterate
      (i:for i from 0 to 15)
      (add-label ret (format nil "R~a" i) i))
    (i:iterate
      (i:with line-num = 0)
      (i:for line next (handler-case
                           (stripped-line stream)
                         (end-of-file () (i:terminate))))
      (unless (cl-ppcre:scan "\\(.*\\)" line)
        (incf line-num))
      (process-line-for-labels ret line line-num))
    (assign-variables ret)
    ret))

(defun print-binary (n)
  (format nil "~16,'0B" n))

(defun compile-a-instruction (instr label-resolution)
  (assert (equal (aref instr 0) #\@))
  (let* ((label-or-value (str:substring 1 nil instr))
         (resolved (cdr (assoc label-or-value (label-to-value label-resolution) :test 'equal))))
    (print-binary (or resolved (parse-integer label-or-value)))))

(defmacro c-instruction-regex ()
  ''(:SEQUENCE
     (:ALTERNATION
      (:SEQUENCE
       (:REGISTER
        (:GREEDY-REPETITION 0 nil
         (:ALTERNATION #\A #\M #\D)))
       "=")
      :VOID)
     (:REGISTER (:GREEDY-REPETITION 0 nil
                 (:INVERTED-CHAR-CLASS #\;)))
     (:ALTERNATION
      (:SEQUENCE
       ";"
       (:REGISTER
        (:GREEDY-REPETITION 0 nil
         :WORD-CHAR-CLASS)))
      :VOID)))

(defparameter *function-to-code*
  '(
    ; With A
      ("0" . "0101010")
      ("1" . "0111111")
     ("-1" . "0111010")
      ("D" . "0001100")
      ("A" . "0110000")
     ("!D" . "0001101")
     ("!A" . "0110001")
     ("-D" . "0001111")
     ("-A" . "0110011")
    ("D+1" . "0011111")
    ("A+1" . "0110111")
    ("D-1" . "0001110")
    ("A-1" . "0110010")
    ("D+A" . "0000010")
    ("D-A" . "0010011")
    ("A-D" . "0000111")
    ("D&A" . "0000000")
    ("D|A" . "0010101")

    ; With M
      ("M" . "1110000")
     ("!M" . "1110001")
     ("-M" . "1110011")
    ("M+1" . "1110111")
    ("M-1" . "1110010")
    ("D+M" . "1000010")
    ("D-M" . "1010011")
    ("M-D" . "1000111")
    ("D&M" . "1000000")
    ("D|M" . "1010101")
    ))


(defmacro assoce (e item alist &rest args)
  (let ((ret (gensym)))
    `(let ((,ret (assoc ,item ,alist ,@args)))
       (if (null ,ret)
           (error ,e)
           ,ret))))

(defun format-comp (comp)
  (cdr (assoce (format nil "invalid comp specification \"~a\"" comp)  comp *function-to-code* :test 'equal)))

(defparameter *dest-order* "ADM")

(defun format-dest (dest)
  (if (null dest)
      "000"
      (progn
        (i:iterate
          (i:for letter in-vector dest)
          (when (not (find letter *dest-order*))
            (error (format nil "invalid destination letter '~a'" letter))))
        (with-output-to-string (out)
          (i:iterate
            (i:for letter in-vector *dest-order*)
            (if (find letter dest)
                (format out "1")
                (format out "0")))))))

(defparameter *jump-lookup*
  '(
    (nil . "000")
    ("JGT" . "001")
    ("JEQ" . "010")
    ("JGE" . "011")
    ("JLT" . "100")
    ("JNE" . "101")
    ("JLE" . "110")
    ("JMP" . "111")
    ))

(defun format-jmp (jmp)
  (cdr (assoce (format nil "invalid jump specification \"~a\"" jmp) jmp *jump-lookup* :test 'equal)))

(defun compile-c-instruction (instr)
  (with-output-to-string (out)
    (trivia:let-match (((trivia.ppcre:ppcre ((c-instruction-regex)) dest comp jmp) instr))
      (format out "111")
      (format out "~a" (format-comp comp))
      (format out "~a" (format-dest dest))
      (format out "~a" (format-jmp jmp)))))

(defun compile-instruction (instr label-resolution)
  (if (equal (aref instr 0) #\@)
      (compile-a-instruction instr label-resolution)
      (compile-c-instruction instr)))

(defun out-file-name (filename)
  (str:replace-all ".asm" ".hack" filename))


(defun assemble (filename)
  (let ((label-resolution (with-open-file (file filename) (generate-label-resolutions file))))
    (with-open-file (file filename)
      (with-open-file (out-file (out-file-name filename) :direction :output :if-exists :overwrite :if-does-not-exist :create)
        (i:iterate
          (i:for next-line = (handler-case (stripped-line file)
                               (end-of-file () (i:terminate))))
          (if (cl-ppcre:scan "\\(.*\\)" next-line)
              (i:next-iteration))
          (write-line (compile-instruction next-line label-resolution) out-file))))))
