;;; meta-lisp-font-lock.el --- Font-lock for meta-lisp -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst meta-lisp--special-forms
  '("define" "lambda" "let" "let*" "letrec" "letrec*"
    "if" "cond" "when" "unless" "and" "or" "else"
    "begin" "match" "match-many"
    "pipe" "chain" "compose"
    "swap"
    "="
    "module" "import" "import-as" "import-all" "private"
    "claim" "claim-type" "admit" "the" "polymorphic"
    "->"
    "define-algebraic-type" "define-record-type"
    "define-struct" "define-struct*" "define-enum"
    "define-type" "define-opaque-type"
    "define-test"
    "declare-primitive-function" "declare-primitive-variable"
    "assert" "assert-not"
    "assert-equal" "assert-not-equal"
    ;; Chinese counterparts (中文语法)
    "定义" "定义测试" "定义类型" "定义枚举"
    "定义代数类型" "定义记录类型" "定义结构" "定义结构*"
    "定义黑盒类型"
    "声明" "声明类型" "承认"
    "声明原始函数" "声明原始变量"
    "模块" "导入" "导入为" "全导入" "私有" "免检"
    "函" "若" "若则" "当" "除非" "且" "或"
    "循序" "令" "递归令" "匹配" "多匹配"
    "管道" "串联" "复合" "型例" "多态" "否则")
  "Special forms in meta-lisp.

These are keywords that appear as the first element of a list
and have special evaluation semantics.  Both the English and the
Chinese syntax are included.")

(defconst meta-lisp--at-forms
  '("@list" "@set" "@hash" "@quote" "@sexp" "@string"
    "@列表" "@集合" "@散列" "@文本" "@引用" "@符号算式")
  "@-prefixed forms that are built-in syntax sugar.

For example: (@list 1 2 3) is sugar for [1 2 3], and
(@文本 \"a\" \"b\") concatenates strings.")

(defconst meta-lisp--builtin-constants
  '("true" "false" "void" "真" "假" "空")
  "Builtin constant names in meta-lisp.")

;;; Helpers

(defun meta-lisp--re-special-forms ()
  "Return a regexp matching any special form at the head of a list."
  (let ((syms meta-lisp--special-forms))
    (concat "(\\(" (regexp-opt syms) "\\)\\_>")))

(defun meta-lisp--re-at-forms ()
  "Return a regexp matching any @-prefixed form at the head of a list."
  (let ((syms meta-lisp--at-forms))
    (concat "(\\(" (regexp-opt syms) "\\)\\_>")))

(defun meta-lisp--re-builtin-constants ()
  "Return a regexp matching builtin constant names."
  (regexp-opt meta-lisp--builtin-constants))

(defconst meta-lisp--name-re
  "\\(?:\\sw\\|\\s_\\)+"
  "Regexp matching a meta-lisp name.

Uses syntax classes from `meta-lisp-mode-syntax-table': word
characters (ASCII letters, digits, and CJK) and symbol
constituents (like - ? ! @ . / $ % &).  This matches both English
names (iadd, point-t) and Chinese names (整数加, 为point, 列表长度).")

(defconst meta-lisp--non-expression-heads
  '(("let" . :structural)
    ("let*" . :structural)
    ("letrec" . :structural)
    ("letrec*" . :structural)
    ("lambda" . :structural)
    ("polymorphic" . :structural)
    ("define" . 1)
    ("claim" . 1)
    ("claim-type" . 1)
    ("admit" . 1)
    ("the" . 1)
    ("define-struct" . 1)
    ("define-struct*" . 1)
    ("define-enum" . 1)
    ("define-type" . 1)
    ("define-opaque-type" . 2)
    ("define-algebraic-type" . 1)
    ("define-record-type" . 1)
    ("define-test" . 1)
    ("module" . :all)
    ("import" . :all)
    ("import-as" . :all)
    ("import-all" . :all)
    ("private" . :all)
    ("exempt" . :all)
    ("declare-primitive-function" . :all)
    ("declare-primitive-variable" . :all)
    ;; Chinese counterparts (中文语法)
    ("函" . :structural)
    ("令" . :structural)
    ("递归令" . :structural)
    ("多态" . :structural)
    ("定义" . 1)
    ("声明" . 1)
    ("声明类型" . 1)
    ("承认" . 1)
    ("型例" . 1)
    ("定义结构" . 1)
    ("定义结构*" . 1)
    ("定义枚举" . 1)
    ("定义类型" . 1)
    ("定义黑盒类型" . 2)
    ("定义代数类型" . 1)
    ("定义记录类型" . 1)
    ("定义测试" . 1)
    ("模块" . :all)
    ("导入" . :all)
    ("导入为" . :all)
    ("全导入" . :all)
    ("私有" . :all)
    ("免检" . :all)
    ("声明原始函数" . :all)
    ("声明原始变量" . :all))
  "Alist mapping keywords to non-expression argument positions.

Values:
  :structural -- arg 1 is a structural list whose direct children
                 have non-expression heads (e.g. let bindings,
                 lambda params, polymorphic type params).
  N (integer) -- first N arguments have non-expression heads
                 (e.g. define's name/header at arg 1).
  :all        -- all arguments have non-expression heads
                 (e.g. module declarations).

Keywords not in this table have all arguments treated as
expressions (e.g. if, when, cond, begin, match, etc.).  for-*
forms are implicitly treated as :structural.")

;;; Non-symbolic faces that are not provided by font-lock

(defface meta-lisp-module-name-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for module name prefixes like `module/' in qualified names."
  :group 'meta-lisp)

(defface meta-lisp-at-form-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for @-prefixed forms like `@list' in meta-lisp."
  :group 'meta-lisp)

(defcustom meta-lisp-highlight-function-calls nil
  "If non-nil, highlight function names in application position.
When enabled, the first element of a function call form like
\(f x y) is highlighted with `font-lock-function-name-face',
unless it is a known special form, @-form, or for-* form."
  :type 'boolean
  :group 'meta-lisp)

;;; Font-lock keywords

(defun meta-lisp--read-head (paren-pos)
  "Return the symbol at the head of the list starting at PAREN-POS.
Returns nil if the first element is not a symbol (e.g. a nested list)."
  (save-excursion
    (goto-char (1+ paren-pos))
    (when (looking-at meta-lisp--name-re)
      (match-string-no-properties 0))))

(defun meta-lisp--lookup-rule (head)
  "Return the non-expression rule for HEAD keyword, or nil.
Handles for-* and 遍历* forms as :structural implicitly."
  (or (cdr (assoc head meta-lisp--non-expression-heads))
      (when (or (string-prefix-p "for-" head)
                (string-prefix-p "遍历" head))
        :structural)))

(defun meta-lisp--arg-index (parent-start form-start)
  "Return 1-based argument index of FORM-START within PARENT-START.
FORM-START must be the position of an opening paren of a sub-form
inside the list at PARENT-START.  The first element (keyword) is
skipped."
  (save-excursion
    (goto-char (1+ parent-start))
    (forward-sexp 1)
    (let ((idx 1)
          (done nil))
      (while (and (not done) (< (point) form-start))
        (let ((end (save-excursion (forward-sexp 1) (point))))
          (if (< form-start end)
              (setq done t)
            (goto-char end)
            (setq idx (1+ idx)))))
      idx)))

(defun meta-lisp--function-call-position-p (form-start)
  "Return non-nil if the list starting at FORM-START is in a function-call
position.  FORM-START is the buffer position of the opening paren."
  (let ((parent-pos (nth 1 (save-excursion (syntax-ppss (max (point-min) (1- form-start)))))))
    (if (not parent-pos)
        t
      (let* ((parent-head (meta-lisp--read-head parent-pos))
             (rule (when parent-head (meta-lisp--lookup-rule parent-head))))
        (cond
         (rule
          (let ((idx (meta-lisp--arg-index parent-pos form-start)))
            (cond
             ((eq rule :all) nil)
             ((eq rule :structural) (not (= idx 1)))
             ((integerp rule) (> idx rule))
             (t t))))
         ((null parent-head)
          (let ((grand-pos (nth 1 (save-excursion (syntax-ppss (max (point-min) (1- parent-pos)))))))
            (if grand-pos
                (let* ((grand-head (meta-lisp--read-head grand-pos))
                       (grand-rule (when grand-head (meta-lisp--lookup-rule grand-head))))
                  (if (and (eq grand-rule :structural)
                           (= (meta-lisp--arg-index grand-pos parent-pos) 1))
                      nil
                    t))
              t)))
         (t t))))))

(defun meta-lisp--match-function-call (limit)
  "Font-lock matcher: highlight function names in application position.
Match (name ...) where name is not a special form, @-form, @comment,
or for-*/遍历* form, and the position is a genuine function-call context."
  (when meta-lisp-highlight-function-calls
    (catch 'meta-lisp--found
      (while (re-search-forward
              (concat "(\\(" meta-lisp--name-re "\\)\\_>")
              limit t)
        (let ((name (match-string-no-properties 1)))
          (unless (or (member name meta-lisp--special-forms)
                      (member name meta-lisp--at-forms)
                      (string= name "@comment")
                      (string= name "@注释")
                      (string-prefix-p "for-" name)
                      (string-prefix-p "遍历" name)
                      (string-prefix-p ":" name))
            (when (save-match-data
                    (meta-lisp--function-call-position-p
                     (match-beginning 0)))
              (throw 'meta-lisp--found t))))))))

(defvar meta-lisp-font-lock-keywords
  `(
   ;; Special forms at head position: (define ...)  (定义 ...)  (lambda ...)
   (,(meta-lisp--re-special-forms)
    1 font-lock-keyword-face)

   ;; Function name: (define (name args ...) body ...)  (定义 (名 参数 ...) ...)
   (,(concat "(\\(?:define\\|定义\\)\\_>\\s-*(\\(" meta-lisp--name-re "\\)")
    1 font-lock-function-name-face)

   ;; Variable / function name: (define name body ...)  (声明 名 型 ...)
   (,(concat "(\\(?:define\\|定义\\|claim\\|声明\\)\\_>\\s-*\\(" meta-lisp--name-re "\\)\\_>")
    1 font-lock-function-name-face)

   ;; @-prefixed forms at head position: (@list ...)  (@列表 ...)  etc.
   (,(meta-lisp--re-at-forms)
    1 'meta-lisp-at-form-face)

   ;; @comment form: structured comment that evaluates to void
   (,(concat "(\\(@comment\\|@注释\\)\\_>")
    1 'font-lock-comment-face)

   ;; for-* / 遍历* special forms at head position:
   ;; (for-list ...)  (遍历列表 ...)  etc.
   (,(concat "(\\(\\(?:for-\\|遍历\\)" meta-lisp--name-re "\\)\\_>")
    1 font-lock-keyword-face)

   ;; Function calls: (f x y) where f is not a special form
   (meta-lisp--match-function-call 1 font-lock-function-name-face)

   ;; Builtin constants as standalone symbols: true  false  void 真 假 空
   (,(concat "\\_<" (meta-lisp--re-builtin-constants) "\\_>")
    0 font-lock-builtin-face)

   ;; Quoted symbols: 'foo '整数型 (before type-names for priority)
   (,(concat "'" meta-lisp--name-re "\\_>")
    0 font-lock-constant-face)

   ;; Keywords: :key :键 (before type-names so :foo-t keeps keyword face)
   (,(concat "\\_<:" meta-lisp--name-re "\\_>")
    0 font-lock-constant-face)

   ;; Type names: symbols ending in -t (int-t  point-t  ...) or
   ;; 型 (整数型 列表型 ...)
   (,(concat "\\_<" meta-lisp--name-re "-t\\_>")
    0 font-lock-type-face)

   (,(concat "\\_<" meta-lisp--name-re "型\\_>")
    0 font-lock-type-face)

   ;; Module prefix: module/ in qualified names like module/name 内置/列表长度
   ;; OVERRIDE=t so it overrides the type face on e.g. builtin/string-t
   (,(concat "\\_<\\(" meta-lisp--name-re "/\\)")
    1 'meta-lisp-module-name-face t)

   ;; Numbers: integers and floats
   (,(concat "\\_<-?[0-9]+\\(\\.[0-9]+\\)?\\_>")
    0 font-lock-constant-face)
   )
  "Default font-lock keywords for `meta-lisp-mode'.")

(provide 'meta-lisp-font-lock)
;;; meta-lisp-font-lock.el ends here
