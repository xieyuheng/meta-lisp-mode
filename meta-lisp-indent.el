;;; meta-lisp-indent.el --- Indentation for meta-lisp -*- lexical-binding: t; -*-

(require 'cl-lib)

;;; Keyword indent specifications
;;; Each keyword entry is (KEYWORD SPEC [BODY-OFFSET]):
;;;   SPEC = number of special args before body (0, 1, or 2)
;;;   BODY-OFFSET = indent offset from opening paren (default 2)

(defvar meta-lisp--keyword-spec-table
  '((module 1)
    (import 1)
    (import-as 0)
    (import-all 0)
    (exempt 0)
    (private 0)
    (claim 1)
    (claim-type 1)
    (admit 1)
    (define 1)
    (declare-primitive-function 1)
    (declare-primitive-variable 1)
    (define-enum 1)
    (define-algebraic-type 1)
    (define-struct 1)
    (define-struct* 1)
    (define-record-type 1)
    (define-test 1)
    (define-type 1)
    (define-opaque-type 2)
    (let 1)
    (let* 1)
    (letrec 1)
    (letrec* 1)
    (= 1)
    (-> 0)
    (the 1)
    (assert 0)
    (assert-not 0)
    (assert-equal 1)
    (assert-not-equal 1)
    (begin 0)
    (lambda 1)
    (match 1)
    (match-many 1)
    (swap 2)
    (pipe 1)
    (chain 0)
    (compose 0)
    (if 1)
    (when 1)
    (unless 1)
    (and 0)
    (or 0)
    (else 0 1)
    (cond 0)
    (@list 0)
    (@set 0)
    (@hash 0)
    (@quote 0)
    (@sexp 0)
    (@string 0)
    (@comment 0)
    (polymorphic 1))
  "Alist mapping keywords to their indent specification.

Each entry is a list (KEYWORD SPEC [BODY-OFFSET]).
SPEC is the number of 'special' arguments to skip before body forms:
  0 = all children are body
  1 = first child is special, rest is body
  2 = first two children are special, rest is body

BODY-OFFSET is the indentation offset from the opening paren for
body forms (default 2).")

;;; Helpers

(defun meta-lisp--at-opening-paren-p (pos)
  "Return t if POS is at an opening paren `(', `[' or `{'."
  (let ((c (char-after pos)))
    (or (eq c ?\() (eq c ?\[) (eq c ?\{))))

(defun meta-lisp--bracket-p (sexp-opening-pos)
  "Return t if SEXP-OPENING-POS opens with `[' or `{'.

These are literal containers, not function-call forms.
They should always use simple body indentation (all elements +2)."
  (let ((c (char-after sexp-opening-pos)))
    (or (eq c ?\[) (eq c ?\{))))

(defun meta-lisp--inside-string-p (state)
  "Return t if STATE indicates we are inside a string."
  (nth 3 state))

(defun meta-lisp--inside-comment-p (state)
  "Return t if STATE indicates we are inside a comment."
  (nth 4 state))

(defun meta-lisp--inside-string-or-comment-p (state)
  "Return t if STATE indicates we are inside a string or comment."
  (or (meta-lisp--inside-string-p state)
      (meta-lisp--inside-comment-p state)))

(defun meta-lisp--enclosing-sexp-opening (state)
  "Return the position of the innermost containing sexp's opening paren.

Returns nil if at top level (not inside any `(' or `[')."
  (nth 1 state))

(defun meta-lisp--read-symbol-around (pos)
  "Read the symbol at or after POS.

Returns a string if a symbol is found at or right after POS,
otherwise returns nil."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t")
    (when (and (not (eobp))
               (or (memq (char-syntax (char-after)) '(?w ?_))
                   (memq (char-after) '(?@))))
      (let ((sym (thing-at-point 'symbol)))
        (when sym
          (substring-no-properties sym))))))

(defun meta-lisp--read-keyword-at-sexp-head (sexp-opening-pos)
  "Read the keyword (first symbol) of the sexp starting at SEXP-OPENING-POS.

Returns the keyword as a string, or nil if the sexp has no keyword."
  (save-excursion
    (goto-char sexp-opening-pos)
    (forward-char 1)   ; skip opening paren
    (meta-lisp--read-symbol-around (point))))

(defun meta-lisp--keyword-spec (keyword)
  "Return the indent spec and body-offset for KEYWORD as a cons (SPEC . OFFSET).

KEYWORD is a string, as returned by `meta-lisp--read-keyword-at-sexp-head'.
Returns nil if KEYWORD is not a known keyword."
  (when keyword
    (let* ((sym (intern keyword))
           (entry (assoc sym meta-lisp--keyword-spec-table)))
      (cond
       (entry (cons (cadr entry) (or (caddr entry) 2)))
       ((string-prefix-p "for-" keyword) (cons 1 2))))))

(defun meta-lisp--forward-over-special-args (sexp-opening-pos n)
  "Move point past the keyword and N special argument sexps.

Point must be at SEXP-OPENING-POS.
After calling, point is at the start of the body elements
\(after skipping the keyword and N arguments\).

Returns the position after skipping, or nil on error."
  (save-excursion
    (goto-char sexp-opening-pos)
    (forward-char 1)           ; skip opening paren
    (condition-case nil
        (progn
          ;; Skip over keyword itself
          (forward-sexp 1)
          ;; Skip over N special argument sexps
          (dotimes (_ n)
            (forward-sexp 1))
          ;; Skip whitespace including newlines
          (skip-chars-forward " \t\n\r")
          (point))
      (error nil))))

(defun meta-lisp--position-in-element-p (pos sexp-opening-pos element-index)
  "Return t if POS falls within element number ELEMENT-INDEX of the sexp.

The keyword is element 0.
ELEMENT-INDEX is 0-based, where 1 is the first argument after keyword."
  (save-excursion
    (goto-char sexp-opening-pos)
    (forward-char 1)           ; skip opening paren
    (condition-case nil
        (progn
          ;; Skip over keyword and preceding arguments
          (dotimes (_ (1+ element-index))
            (forward-sexp 1))
          ;; Now we're at the end of element ELEMENT-INDEX.
          ;; Check if POS is between the start and end.
          (let* ((end (point))
                 (start
                  (progn
                    (goto-char sexp-opening-pos)
                    (forward-char 1)
                    (dotimes (_ element-index)
                      (forward-sexp 1))
                    (skip-chars-forward " \t")
                    (point))))
            (and (>= pos start) (< pos end))))
      (error nil))))

;;; Bracket indent computation

(defun meta-lisp--compute-bracket-indent (containing-pos)
  "Compute indentation for bracket containers [ ... ] and { ... }.

All elements align with the first element."
  (save-excursion
    (goto-char containing-pos)
    (forward-char 1)                     ; skip [ or {
    (skip-chars-forward " \t")           ; skip to first element
    (if (eobp)
        (+ (current-column) 2)           ; empty bracket
      (current-column))))                ; align with first element

;;; Body indent computation

(defun meta-lisp--compute-body-indent (containing-pos body-start-pos offset)
  "Compute the body indentation for a sexp containing the current line.

CONTAINING-POS is the position of the opening paren of the
containing sexp.  BODY-START-POS is the position of the first
body element (after skipping the keyword and any special args).
OFFSET is the indentation offset from the opening paren.

Returns the column number to indent to."
  (save-excursion
    (goto-char body-start-pos)
    ;; Check if the first body element is on the same line as the keyword
    (let* ((containing-line (line-number-at-pos containing-pos))
           (body-line (line-number-at-pos body-start-pos)))
      (if (= containing-line body-line)
          ;; Body starts on same line as keyword -- align subsequent
          ;; body elements with the first body element
          (current-column)
        ;; Body starts on a new line -- indent OFFSET from opening paren
        (+ (save-excursion
             (goto-char containing-pos)
             (current-column))
           offset)))))

;;; Default (function-call) indent computation

(defun meta-lisp--compute-function-indent (containing-pos)
  "Compute indentation for a non-keyword (function-call) sexp.

CONTAINING-POS is the position of the opening paren.

If the first element is a list, align subsequent elements with it.
If the first element is an atom (symbol), align subsequent elements
with the first argument."
  (save-excursion
    (goto-char containing-pos)
    (forward-char 1)                    ; skip opening paren
    (condition-case nil
        (progn
          (skip-chars-forward " \t")
          (let ((first-col (current-column)))
            (if (meta-lisp--at-opening-paren-p (point))
                ;; Car is a list -- align with it
                first-col
              ;; Car is an atom -- align with first argument
              (forward-sexp 1)            ; skip the atom
              (skip-chars-forward " \t\n\r") ; skip whitespace to first arg
              (if (eobp)
                  (+ first-col 2)
                (if (= (line-number-at-pos containing-pos)
                       (line-number-at-pos (point)))
                    ;; First arg on same line -- align with it
                    (current-column)
                  ;; First arg on its own line -- indent 2
                  (+ (save-excursion
                       (goto-char containing-pos)
                       (current-column))
                     2))))))
      (error
       (+ (save-excursion
            (goto-char containing-pos)
            (current-column))
          2)))))

;;; Main indent computation

(defun meta-lisp--compute-indent (pos state)
  "Compute indentation column for POS given parse STATE.

Returns the column number, or nil (meaning don't change indentation)."
  (cond
   ;; Top level: no indentation
   ((null (meta-lisp--enclosing-sexp-opening state))
    0)

   ;; Inside string or comment: don't change
   ((meta-lisp--inside-string-or-comment-p state)
    nil)

    (t
     (let* ((containing (meta-lisp--enclosing-sexp-opening state)))
       (cond
         ;; Brackets [] and {} -- align with first element
         ((meta-lisp--bracket-p containing)
          (meta-lisp--compute-bracket-indent containing))

        (t
         (let* ((keyword (meta-lisp--read-keyword-at-sexp-head containing))
                (spec-pair (meta-lisp--keyword-spec keyword)))
           (cond
            ((null spec-pair)
             ;; Not a keyword -- use function-call indentation
             (meta-lisp--compute-function-indent containing))

            (t
             ;; Keyword with spec
             (let* ((spec (car spec-pair))
                    (body-offset (cdr spec-pair))
                    (body-start (meta-lisp--forward-over-special-args containing spec)))
               (cond
                ((null body-start)
                 ;; Malformed -- fall back to OFFSET from opening paren
                 (+ (save-excursion
                      (goto-char containing)
                      (current-column))
                    body-offset))

                ;; If we're in the body region, use body indentation
                ((>= pos body-start)
                 (meta-lisp--compute-body-indent containing body-start body-offset))

                ;; Otherwise, we're in a special arg -- use default alignment
                (t
                 (meta-lisp--compute-function-indent containing)))))))))))))

;;; Public API

(defun meta-lisp-indent-line ()
  "Indent the current line as meta-lisp code."
  (interactive)
  (let* ((target (save-excursion (back-to-indentation) (point)))
         (state (syntax-ppss target)))
    (if (meta-lisp--inside-string-p state)
        ;; Inside a string -- don't change indentation
        (when (= (current-column) (current-indentation))
          ;; But if at line start inside a string, maintain context
          (save-excursion
            (goto-char (nth 8 state))
            (indent-line-to (current-column))))
      (let ((indent (meta-lisp--compute-indent target state)))
        (when indent
          (indent-line-to indent))))))

(provide 'meta-lisp-indent)
;;; meta-lisp-indent.el ends here
