;;; meta-lisp-test.el --- Tests for meta-lisp-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'meta-lisp-mode)

;;; Helpers

(defun meta-lisp-test--indent (source)
  "Insert SOURCE into a meta-lisp-mode buffer, indent it, return result."
  (with-temp-buffer
    (meta-lisp-mode)
    (insert source)
    (indent-region (point-min) (point-max))
    (buffer-string)))

(defun meta-lisp-test--font-lock-at (source)
  "Return the face at the position marked by § in SOURCE.

SOURCE is meta-lisp code containing exactly one § character
marking the position to check. The buffer is fontified and the
face at that position is returned."
  (with-temp-buffer
    (meta-lisp-mode)
    (let ((pos (string-match "§" source)))
      (unless pos
        (error "Missing § marker in test source"))
      (insert (replace-regexp-in-string "§" "" source))
      (font-lock-mode 1)
      (font-lock-ensure)
      (let ((face (get-text-property (+ (point-min) pos) 'face)))
        (when face
          (if (consp face) (car face) face))))))

;;; Indentation tests -- top-level

(ert-deftest meta-lisp-indent-top-level ()
  "Top-level forms should have no indentation."
  (should (equal (meta-lisp-test--indent "(module example)")
                 "(module example)"))
  (should (equal (meta-lisp-test--indent "(define x 1)")
                 "(define x 1)")))

;;; Indentation -- spec=1 keywords

(ert-deftest meta-lisp-indent-define-fn ()
  "define with function form: body indented 2 from opening paren."
  (let ((result (meta-lisp-test--indent
                 "(define (f x)\n(println x)\n(iadd x 1))")))
    (should (equal result "(define (f x)\n  (println x)\n  (iadd x 1))"))))

(ert-deftest meta-lisp-indent-define-var ()
  "define with variable: body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(define answer\n42)")))
    (should (equal result "(define answer\n  42)"))))

(ert-deftest meta-lisp-indent-lambda ()
  "lambda: params special, body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(lambda (x y)\n(iadd x y))")))
    (should (equal result "(lambda (x y)\n  (iadd x y))"))))

(ert-deftest meta-lisp-indent-let ()
  "let: bindings special, body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(let ((x 1)\n(y 2))\n(iadd x y))")))
    (should (equal result "(let ((x 1)\n      (y 2))\n  (iadd x y))"))))

(ert-deftest meta-lisp-indent-let-star ()
  "let*: same as let."
  (let ((result (meta-lisp-test--indent
                 "(let* ((x 1)\n(y (iadd x 1)))\n(iadd x y))")))
    (should (equal result "(let* ((x 1)\n       (y (iadd x 1)))\n  (iadd x y))"))))

(ert-deftest meta-lisp-indent-if ()
  "if: condition special, branches indented 2."
  (let ((result (meta-lisp-test--indent
                 "(if (equal? x 0)\n'zero\n'non-zero)")))
    (should (equal result "(if (equal? x 0)\n  'zero\n  'non-zero)"))))

(ert-deftest meta-lisp-indent-match ()
  "match: target special, clauses indented 2."
  (let ((result (meta-lisp-test--indent
                 "(match exp\n((var-exp name)\nbody)\n((apply-exp target arg)\nbody2))")))
    (should (equal result "(match exp\n  ((var-exp name)\n   body)\n  ((apply-exp target arg)\n   body2))"))))

(ert-deftest meta-lisp-indent-claim ()
  "claim: name special, body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(claim add1\n(-> int-t int-t))")))
    (should (equal result "(claim add1\n  (-> int-t int-t))"))))

(ert-deftest meta-lisp-indent-when-unless ()
  "when and unless: condition special, body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(when debug?\n(print \"debug\")\n(newline))")))
    (should (equal result "(when debug?\n  (print \"debug\")\n  (newline))"))))

;;; Indentation -- spec=0 keywords

(ert-deftest meta-lisp-indent-cond ()
  "cond: all clauses indented 2 from opening paren."
  (let ((result (meta-lisp-test--indent
                 "(cond\n((equal? x 1) 'one)\n(else 'other))")))
    (should (equal result "(cond\n  ((equal? x 1) 'one)\n  (else 'other))"))))

(ert-deftest meta-lisp-indent-begin ()
  "begin: all body forms indented 2."
  (let ((result (meta-lisp-test--indent
                 "(begin\n(println \"step 1\")\n(println \"step 2\")\n42)")))
    (should (equal result "(begin\n  (println \"step 1\")\n  (println \"step 2\")\n  42)"))))

(ert-deftest meta-lisp-indent-and ()
  "and: all forms indented 2."
  (let ((result (meta-lisp-test--indent
                 "(and\n(int? x)\n(int-positive? x))")))
    (should (equal result "(and\n  (int? x)\n  (int-positive? x))"))))

(ert-deftest meta-lisp-indent-or ()
  "or: all forms indented 2."
  (let ((result (meta-lisp-test--indent
                 "(or\n(equal? x 0)\n(equal? x 1))")))
    (should (equal result "(or\n  (equal? x 0)\n  (equal? x 1))"))))

(ert-deftest meta-lisp-indent-assert ()
  "assert-equal: all forms indented 2."
  (let ((result (meta-lisp-test--indent
                 "(assert-equal\n2\n(iadd 1 1))")))
    (should (equal result "(assert-equal\n  2\n  (iadd 1 1))"))))

;;; Indentation -- function calls

(ert-deftest meta-lisp-indent-function-call ()
  "Function call: arguments indent +2 from opening paren."
  (let ((result (meta-lisp-test--indent
                 "(iadd 1\n2)")))
    (should (equal result "(iadd 1\n  2)"))))

(ert-deftest meta-lisp-indent-function-call-newline ()
  "Function call: function on its own line."
  (let ((result (meta-lisp-test--indent
                 "(iadd\n1\n2)")))
    (should (equal result "(iadd\n  1\n  2)"))))

;;; Indentation -- brackets [] and {}

(ert-deftest meta-lisp-indent-brackets ()
  "Brackets inside define: bracket body indented 2 from define."
  (let ((result (meta-lisp-test--indent
                 "(define xs\n[1 2 3])")))
    (should (equal result "(define xs\n  [1 2 3])"))))

(ert-deftest meta-lisp-indent-brackets-fn-call ()
  "Brackets in function calls: elements indented 2 from bracket."
  (let ((result (meta-lisp-test--indent
                 "(list-get-element\n[1 2 3]\n0)")))
    (should (equal result "(list-get-element\n  [1 2 3]\n  0)"))))

(ert-deftest meta-lisp-indent-brackets-multiline ()
  "Multi-line bracket literal: all elements align with first."
  (let ((result (meta-lisp-test--indent
                 "[1\n2\n3]")))
    (should (equal result "[1\n 2\n 3]"))))

(ert-deftest meta-lisp-indent-braces ()
  "Curly braces: all elements align with first."
  (let ((result (meta-lisp-test--indent
                 "{:a 1\n:b 2\n:c 3}")))
    (should (equal result "{:a 1\n :b 2\n :c 3}"))))

(ert-deftest meta-lisp-indent-brackets-nested ()
  "Brackets nested in paren forms."
  (let ((result (meta-lisp-test--indent
                 "(let ((xs [1\n2\n3]))\n(car xs))")))
    (should (equal result
                   "(let ((xs [1\n           2\n           3]))\n  (car xs))"))))

;;; Indentation -- nested

(ert-deftest meta-lisp-indent-nested-define ()
  "Nested defines should indent correctly."
  (let ((result (meta-lisp-test--indent
                 "(define (evaluate exp env)\n(match exp\n((var-exp name)\n(env-lookup name env)))))")))
    (should (equal result
                   "(define (evaluate exp env)\n  (match exp\n    ((var-exp name)\n     (env-lookup name env)))))"))))

(ert-deftest meta-lisp-indent-nested-let ()
  "Nested let forms."
  (let ((result (meta-lisp-test--indent
                 "(let ((x 1)\n(y 2))\n(let ((z 3))\n(iadd x z)))")))
    (should (equal result
                   "(let ((x 1)\n      (y 2))\n  (let ((z 3))\n    (iadd x z)))"))))

(ert-deftest meta-lisp-indent-define-test ()
  "define-test: name special, body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(define-test my-test\n(assert-equal 2 (iadd 1 1))\n(assert true))")))
    (should (equal result
                   "(define-test my-test\n  (assert-equal 2 (iadd 1 1))\n  (assert true))"))))

(ert-deftest meta-lisp-indent-module-import ()
  "module and import forms."
  (let ((result (meta-lisp-test--indent
                 "(module my-module)\n(import math\npi\ncircumference)")))
    (should (equal result
                   "(module my-module)\n(import math\n  pi\n  circumference)"))))

(ert-deftest meta-lisp-indent-define-struct ()
  "define-struct: type-name special, fields indented 2."
  (let ((result (meta-lisp-test--indent
                 "(define-struct point-t\n(x float-t)\n(y float-t))")))
    (should (equal result
                   "(define-struct point-t\n  (x float-t)\n  (y float-t))"))))

(ert-deftest meta-lisp-indent-define-enum ()
  "define-enum: type-name special."
  (let ((result (meta-lisp-test--indent
                 "(define-enum exp-t\n(var-exp (name symbol-t))\n(apply-exp (target exp-t) (arg exp-t)))")))
    (should (equal result
                   "(define-enum exp-t\n  (var-exp (name symbol-t))\n  (apply-exp (target exp-t) (arg exp-t)))"))))

;;; Font-lock tests

(ert-deftest meta-lisp-font-lock-keyword ()
  "Special forms should use font-lock-keyword-face."
  (should (eq (meta-lisp-test--font-lock-at "(§define x 1)")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§lambda (x) x)")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§let ((x 1)) x)")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§if true 1 2)")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§-> int-t int-t)")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§else 42)")
              'font-lock-keyword-face)))

(ert-deftest meta-lisp-font-lock-function-name ()
  "define's name should use font-lock-function-name-face."
  (should (eq (meta-lisp-test--font-lock-at "(define (§f x) x)")
              'font-lock-function-name-face))
  (should (eq (meta-lisp-test--font-lock-at "(define §answer 42)")
              'font-lock-function-name-face))
  (should (eq (meta-lisp-test--font-lock-at "(claim §add1 (-> int-t int-t))")
              'font-lock-function-name-face)))

(ert-deftest meta-lisp-font-lock-at-form ()
  "@-prefixed forms should use meta-lisp-at-form-face."
  (let ((face (meta-lisp-test--font-lock-at "(§@list 1 2 3)")))
    (should (eq face 'meta-lisp-at-form-face))))

(ert-deftest meta-lisp-font-lock-at-comment ()
  "@comment should use font-lock-comment-face."
  (let ((face (meta-lisp-test--font-lock-at "(§@comment (lambda (x) x))")))
    (should (eq face 'font-lock-comment-face))))

(ert-deftest meta-lisp-font-lock-declare-primitive ()
  "declare-primitive forms should use font-lock-keyword-face."
  (should (eq (meta-lisp-test--font-lock-at "(§declare-primitive-function add1 1)")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§declare-primitive-variable pi)")
              'font-lock-keyword-face)))

(ert-deftest meta-lisp-font-lock-builtin-constant ()
  "Builtin constants should use font-lock-builtin-face."
  (let ((face (meta-lisp-test--font-lock-at "(if §true 1 2)")))
    (should (eq face 'font-lock-builtin-face)))
  (let ((face (meta-lisp-test--font-lock-at "(if §false 1 2)")))
    (should (eq face 'font-lock-builtin-face)))
  (let ((face (meta-lisp-test--font-lock-at "§void")))
    (should (eq face 'font-lock-builtin-face))))

(ert-deftest meta-lisp-font-lock-type ()
  "Type names (symbols ending in -t) should use font-lock-type-face."
  (let ((face (meta-lisp-test--font-lock-at "(claim x §point-t)")))
    (should (eq face 'font-lock-type-face))))

(ert-deftest meta-lisp-font-lock-keyword-no-partial ()
  "Keywords inside larger symbols should NOT trigger keyword face."
  (should-not (eq (meta-lisp-test--font-lock-at "(§lambda-term ...)")
                  'font-lock-keyword-face))
  (should-not (eq (meta-lisp-test--font-lock-at "(§let1-term ...)")
                  'font-lock-keyword-face))
  (should-not (eq (meta-lisp-test--font-lock-at "(§if-term ...)")
                  'font-lock-keyword-face))
  (should-not (eq (meta-lisp-test--font-lock-at "(§all-term ...)")
                  'font-lock-keyword-face)))

(ert-deftest meta-lisp-font-lock-keyword-constant ()
  "Keyword symbols (:xxx) should use font-lock-constant-face."
  (let ((face (meta-lisp-test--font-lock-at "(@hash §:a 1 :b 2)")))
    (should (eq face 'font-lock-constant-face))))

(ert-deftest meta-lisp-font-lock-module-prefix ()
  "Module prefix in qualified names should use module-name-face."
  (let ((face (meta-lisp-test--font-lock-at "(§sigma/pi 3.14)")))
    (should (eq face 'meta-lisp-module-name-face))))

(ert-deftest meta-lisp-font-lock-type-qualified ()
  "Qualified type: suffix gets type face, prefix gets module face."
  (let ((face (meta-lisp-test--font-lock-at "(math/§point-t x y)")))
    (should (eq face 'font-lock-type-face))))

;;; Comment tests

(ert-deftest meta-lisp-comment-syntax ()
  "Comments should be properly recognized."
  (with-temp-buffer
    (meta-lisp-mode)
    (insert ";; this is a comment\n(define x 1)")
    (goto-char 3)
    (should (nth 4 (syntax-ppss)))))

;;; S-expression tests

(ert-deftest meta-lisp-sexp-navigation ()
  "forward-sexp should work with both () and []."
  (with-temp-buffer
    (meta-lisp-mode)
    (insert "(define xs [1 2 3])")
    (goto-char (point-min))
    (forward-sexp 1)
    (should (eobp))))

(ert-deftest meta-lisp-font-lock-function-call ()
  "Function calls in application position should use font-lock-function-name-face."
  (let ((old meta-lisp-highlight-function-calls))
    (setq meta-lisp-highlight-function-calls t)
    (unwind-protect
        (progn
          (should (eq (meta-lisp-test--font-lock-at "(§iadd 1 2)")
                      'font-lock-function-name-face))
          (should (eq (meta-lisp-test--font-lock-at "(§println \"hello\")")
                      'font-lock-function-name-face))
          (should (eq (meta-lisp-test--font-lock-at "(§car xs)")
                      'font-lock-function-name-face))
          ;; Special forms still get keyword face, not function face
          (should (eq (meta-lisp-test--font-lock-at "(§lambda (x) x)")
                      'font-lock-keyword-face))
          (should (eq (meta-lisp-test--font-lock-at "(§let ((x 1)) x)")
                      'font-lock-keyword-face))
          (should (eq (meta-lisp-test--font-lock-at "(§define (f x) x)")
                      'font-lock-keyword-face))
          ;; for-* still gets keyword face
          (should (eq (meta-lisp-test--font-lock-at "(§for-list (x xs) x)")
                      'font-lock-keyword-face)))
      (setq meta-lisp-highlight-function-calls old))))

(ert-deftest meta-lisp-font-lock-function-call-context ()
  "Heads in non-expression positions should NOT get function-name-face."
  (let ((old meta-lisp-highlight-function-calls))
    (setq meta-lisp-highlight-function-calls t)
    (unwind-protect
        (progn
          ;; lambda param list
          (should-not (eq (meta-lisp-test--font-lock-at "(lambda (§pair) pair)")
                          'font-lock-function-name-face))
          ;; let binding variable
          (should-not (eq (meta-lisp-test--font-lock-at "(let ((§x 1)) x)")
                          'font-lock-function-name-face))
          ;; all type param
          (should-not (eq (meta-lisp-test--font-lock-at "(all (§A) A)")
                          'font-lock-function-name-face))
          ;; define function header name (highlighted by existing define-name rule)
          (should (eq (meta-lisp-test--font-lock-at "(define (§f x) (g x))")
                      'font-lock-function-name-face))
          ;; declare-primitive: all args are non-expression
          (should-not (eq (meta-lisp-test--font-lock-at "(declare-primitive-function §add1 1)")
                          'font-lock-function-name-face))
          ;; define name (highlighted by existing define-name rule)
          (should (eq (meta-lisp-test--font-lock-at "(define (§my-fn x) x)")
                      'font-lock-function-name-face))
          ;; --- These SHOULD be highlighted (expression positions) ---
          ;; let binding VALUE expression
          (should (eq (meta-lisp-test--font-lock-at "(let ((x (§f 1))) x)")
                      'font-lock-function-name-face))
          ;; let body
          (should (eq (meta-lisp-test--font-lock-at "(let ((x 1)) (§g x))")
                      'font-lock-function-name-face))
          ;; lambda body
          (should (eq (meta-lisp-test--font-lock-at "(lambda (x) (§iadd x 1))")
                      'font-lock-function-name-face))
          ;; all body
          (should (eq (meta-lisp-test--font-lock-at "(all (A) (§id A))")
                      'font-lock-function-name-face))
          ;; deep nesting: let binding value expression
          (should (eq (meta-lisp-test--font-lock-at "(let ((x (§car (§cdr y)))) x)")
                      'font-lock-function-name-face)))
      (setq meta-lisp-highlight-function-calls old))))

(ert-deftest meta-lisp-font-lock-function-call-disabled ()
  "When the option is disabled, function calls should have no face."
  (let ((old meta-lisp-highlight-function-calls))
    (setq meta-lisp-highlight-function-calls nil)
    (unwind-protect
        (should-not (eq (meta-lisp-test--font-lock-at "(§iadd 1 2)")
                        'font-lock-function-name-face))
      (setq meta-lisp-highlight-function-calls old))))

;;; Chinese syntax tests (中文语法)

;;; Indentation -- Chinese keywords

(ert-deftest meta-lisp-indent-zh-define-fn ()
  "定义 with function form: body indented 2 from opening paren."
  (let ((result (meta-lisp-test--indent
                 "(定义 (平方 x)\n(整数乘 x x))")))
    (should (equal result "(定义 (平方 x)\n  (整数乘 x x))"))))

(ert-deftest meta-lisp-indent-zh-define-var ()
  "定义 with variable: body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(定义 answer\n42)")))
    (should (equal result "(定义 answer\n  42)"))))

(ert-deftest meta-lisp-indent-zh-lambda ()
  "函: params special, body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(函 (x)\n(整数加 x 1))")))
    (should (equal result "(函 (x)\n  (整数加 x 1))"))))

(ert-deftest meta-lisp-indent-zh-claim ()
  "声明: name special, type indented 2."
  (let ((result (meta-lisp-test--indent
                 "(声明 add1\n(-> 整数型 整数型))")))
    (should (equal result "(声明 add1\n  (-> 整数型 整数型))"))))

(ert-deftest meta-lisp-indent-zh-let ()
  "令: bindings aligned, body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(令 ((x 1)\n(y 2))\n(整数加 x y))")))
    (should (equal result "(令 ((x 1)\n     (y 2))\n  (整数加 x y))"))))

(ert-deftest meta-lisp-indent-zh-letrec ()
  "递归令: same as let."
  (let ((result (meta-lisp-test--indent
                 "(递归令 ((f (函 (n) n)))\n(f 1))")))
    (should (equal result "(递归令 ((f (函 (n) n)))\n  (f 1))"))))

(ert-deftest meta-lisp-indent-zh-if ()
  "若: condition special, branches indented 2."
  (let ((result (meta-lisp-test--indent
                 "(若 (整数小于 x 0)\n(整数负 x)\nx)")))
    (should (equal result "(若 (整数小于 x 0)\n  (整数负 x)\n  x)"))))

(ert-deftest meta-lisp-indent-zh-cond ()
  "若则 with 否则: clauses indented 2 from opening paren."
  (let ((result (meta-lisp-test--indent
                 "(若则\n((为整数 x) 1)\n(否则 2))")))
    (should (equal result "(若则\n  ((为整数 x) 1)\n  (否则 2))"))))

(ert-deftest meta-lisp-indent-zh-when-unless ()
  "当 and 除非: condition special, body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(当 debug\n(打印 \"debug\")\n(换行))")))
    (should (equal result "(当 debug\n  (打印 \"debug\")\n  (换行))")))
  (let ((result (meta-lisp-test--indent
                 "(除非 debug\n(打印 \"debug\"))")))
    (should (equal result "(除非 debug\n  (打印 \"debug\"))"))))

(ert-deftest meta-lisp-indent-zh-match ()
  "匹配: target special, clauses indented 2."
  (let ((result (meta-lisp-test--indent
                 "(匹配 exp\n((var-exp name)\nbody)\n((apply-exp target arg)\nbody2))")))
    (should (equal result "(匹配 exp\n  ((var-exp name)\n   body)\n  ((apply-exp target arg)\n   body2))"))))

(ert-deftest meta-lisp-indent-zh-begin ()
  "循序: all body forms indented 2."
  (let ((result (meta-lisp-test--indent
                 "(循序\n(打印行 \"step 1\")\n42)")))
    (should (equal result "(循序\n  (打印行 \"step 1\")\n  42)"))))

(ert-deftest meta-lisp-indent-zh-pipe ()
  "管道: target special, steps indented 2."
  (let ((result (meta-lisp-test--indent
                 "(管道 5\n加一\n平方)")))
    (should (equal result "(管道 5\n  加一\n  平方)"))))

(ert-deftest meta-lisp-indent-zh-define-test ()
  "定义测试: name special, body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(定义测试 平方测试\n(断言相等 4 (平方 2)))")))
    (should (equal result "(定义测试 平方测试\n  (断言相等 4 (平方 2)))"))))

(ert-deftest meta-lisp-indent-zh-define-struct ()
  "定义结构: type-name special, fields indented 2."
  (let ((result (meta-lisp-test--indent
                 "(定义结构 point-t\n(x 浮点型)\n(y 浮点型))")))
    (should (equal result "(定义结构 point-t\n  (x 浮点型)\n  (y 浮点型))"))))

(ert-deftest meta-lisp-indent-zh-define-opaque ()
  "定义黑盒类型: two special args, body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(定义黑盒类型 (匣子型 E) (列表型 E)\n(作匣子 (-> (匣子型 E))))")))
    (should (equal result
                   "(定义黑盒类型 (匣子型 E) (列表型 E)\n  (作匣子 (-> (匣子型 E))))"))))

(ert-deftest meta-lisp-indent-zh-import ()
  "导入: module special, names indented 2."
  (let ((result (meta-lisp-test--indent
                 "(导入 math\npi\ncircumference)")))
    (should (equal result "(导入 math\n  pi\n  circumference)"))))

(ert-deftest meta-lisp-indent-zh-for ()
  "遍历列表: like for-* forms, body indented 2."
  (let ((result (meta-lisp-test--indent
                 "(遍历列表 (x xs)\nx)")))
    (should (equal result "(遍历列表 (x xs)\n  x)"))))

(ert-deftest meta-lisp-indent-zh-top-level ()
  "Chinese top-level forms should have no indentation."
  (should (equal (meta-lisp-test--indent "(模块 示例)")
                 "(模块 示例)")))

;;; Font-lock -- Chinese keywords

(ert-deftest meta-lisp-font-lock-zh-keyword ()
  "Chinese special forms should use font-lock-keyword-face."
  (should (eq (meta-lisp-test--font-lock-at "(§定义 x 1)")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§函 (x) x)")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§若 真 1 2)")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§若则 ((为整数 x) 1) (否则 2))")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§模块 example)")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§定义测试 t1 (断言 真))")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§定义类型 my-t (x 整数型))")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§声明基本函数 iadd 2)")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§遍历列表 (x xs) x)")
              'font-lock-keyword-face))
  (should (eq (meta-lisp-test--font-lock-at "(§否则 42)")
              'font-lock-keyword-face)))

(ert-deftest meta-lisp-font-lock-zh-function-name ()
  "Chinese define/claim names should use font-lock-function-name-face."
  (should (eq (meta-lisp-test--font-lock-at "(定义 (§平方 x) (整数乘 x x))")
              'font-lock-function-name-face))
  (should (eq (meta-lisp-test--font-lock-at "(定义 §answer 42)")
              'font-lock-function-name-face))
  (should (eq (meta-lisp-test--font-lock-at "(声明 §add1 (-> 整数型 整数型))")
              'font-lock-function-name-face)))

(ert-deftest meta-lisp-font-lock-zh-at-form ()
  "@-prefixed Chinese forms should use meta-lisp-at-form-face."
  (let ((face (meta-lisp-test--font-lock-at "(§@文本 \"a\" \"b\")")))
    (should (eq face 'meta-lisp-at-form-face)))
  (let ((face (meta-lisp-test--font-lock-at "(§@列表 1 2 3)")))
    (should (eq face 'meta-lisp-at-form-face))))

(ert-deftest meta-lisp-font-lock-zh-at-comment ()
  "@注释 should use font-lock-comment-face."
  (let ((face (meta-lisp-test--font-lock-at "(§@注释 (函 (x) x))")))
    (should (eq face 'font-lock-comment-face))))

(ert-deftest meta-lisp-font-lock-zh-builtin-constant ()
  "Chinese builtin constants should use font-lock-builtin-face."
  (let ((face (meta-lisp-test--font-lock-at "(若 §真 1 2)")))
    (should (eq face 'font-lock-builtin-face)))
  (let ((face (meta-lisp-test--font-lock-at "(若 §假 1 2)")))
    (should (eq face 'font-lock-builtin-face)))
  (let ((face (meta-lisp-test--font-lock-at "§空")))
    (should (eq face 'font-lock-builtin-face))))

(ert-deftest meta-lisp-font-lock-zh-type ()
  "Chinese type names ending in 型 should use font-lock-type-face."
  (let ((face (meta-lisp-test--font-lock-at "(声明 x §整数型)")))
    (should (eq face 'font-lock-type-face)))
  (let ((face (meta-lisp-test--font-lock-at "(声明 x §列表型)")))
    (should (eq face 'font-lock-type-face)))
  (let ((face (meta-lisp-test--font-lock-at "(声明 x §point-t)")))
    (should (eq face 'font-lock-type-face))))

(ert-deftest meta-lisp-font-lock-zh-module-prefix ()
  "Chinese module prefix in qualified names should use module-name-face."
  (let ((face (meta-lisp-test--font-lock-at "(§内置/列表长度 [1 2])")))
    (should (eq face 'meta-lisp-module-name-face))))

(ert-deftest meta-lisp-font-lock-zh-keyword-symbol ()
  "Chinese keyword symbols (:键) should use font-lock-constant-face."
  (let ((face (meta-lisp-test--font-lock-at "(@散列 §:键 1)")))
    (should (eq face 'font-lock-constant-face))))

(ert-deftest meta-lisp-font-lock-zh-quoted ()
  "Quoted Chinese type names should get constant face, not type face."
  (let ((face (meta-lisp-test--font-lock-at "'§整数型")))
    (should (eq face 'font-lock-constant-face))))

(ert-deftest meta-lisp-font-lock-zh-no-partial ()
  "Chinese keywords inside larger symbols should NOT trigger keyword face."
  (should-not (eq (meta-lisp-test--font-lock-at "(§定义器 x)")
                  'font-lock-keyword-face))
  (should-not (eq (meta-lisp-test--font-lock-at "(§声明器 x)")
                  'font-lock-keyword-face)))

(ert-deftest meta-lisp-font-lock-zh-function-call ()
  "Chinese function calls should use font-lock-function-name-face."
  (let ((old meta-lisp-highlight-function-calls))
    (setq meta-lisp-highlight-function-calls t)
    (unwind-protect
        (progn
          (should (eq (meta-lisp-test--font-lock-at "(§整数加 1 2)")
                      'font-lock-function-name-face))
          (should (eq (meta-lisp-test--font-lock-at "(§为point p)")
                      'font-lock-function-name-face))
          (should (eq (meta-lisp-test--font-lock-at "(§列表长度 [1 2])")
                      'font-lock-function-name-face))
          ;; Chinese special forms still get keyword face
          (should (eq (meta-lisp-test--font-lock-at "(§定义 (f x) x)")
                      'font-lock-keyword-face))
          (should (eq (meta-lisp-test--font-lock-at "(§遍历列表 (x xs) x)")
                      'font-lock-keyword-face)))
      (setq meta-lisp-highlight-function-calls old))))

(provide 'meta-lisp-test)
;;; meta-lisp-test.el ends here
