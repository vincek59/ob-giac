;;; giac-mode.el --- major mode for giac  -*- lexical-binding: t; -*-

(require 'compile)
(require 'cc-mode)
(require 'cc-langs)

(eval-when-compile
  (require 'cc-fonts)
  (require 'rx))



;;utils

(defun myC(lst)
  (if (equal (length lst) 0) nil
    (if (equal (length lst) 1) (car lst)
			   (concat (car lst) "(\\|" (myC (cdr lst )))
			   )
    )
  )

;;; create inherited mako-mode from c-mode
(eval-and-compile (c-add-language 'giac-mode 'c-mode))
(defvar giac-mode-syntax-table nil)

(c-lang-defconst c-assignment-operators
  giac   '(":=" "=>" "=<"))



(c-lang-defconst c-make-mode-syntax-table
       giac `(lambda ()
                  (let ((table (make-syntax-table)))
                    (c-populate-syntax-table table)
                    (modify-syntax-entry ?@ "_" table)
                    table)))

  ;; Constants.                                                                                   
(c-lang-defconst c-constant-kwds
  giac '( "i" "e" "pi" "false" "true" "infinity")) 

;; The block comment starter is (*.                                                             
(c-lang-defconst c-block-comment-starter
  giac "/*")

;; The block comment ender is *).                                                               
(c-lang-defconst c-block-comment-ender
  giac "*/")

(defconst giac-keywords '("latex" "integrate" "diff" "factor" "int" "Int" "expand" "eigenvalues" "eigenvects" "developper" "simplifier" "simplify" "deriver" "unapply" "factoriser" "integrer" "series") "Liste de quelques mots clef de giac")

(defconst giac-functions-names '("cos" "sin" "tan" "cosh" "sinh" "exp" "ln" "log" "tanh" "atan" "acos" "asin" "sqrt" "acosh" "asinh" "atanh" "floor" "ceil") "Liste de quelques fonctions de giac")


; (defcustom giac-font-lock-extra-types nil
;   "Extra types to recognize in giac mode.")


(c-lang-defconst c-other-kwds
   giac giac-keywords)


;; (c-lang-defconst c-paren-type-kwds
;;   giac
;;   (append 
;;     giac-functions-names 
;;    (c-lang-const c-paren-type-kwds) 
;;    nil))


;(setq giac-builtins-regexp (regexp-opt giac-functions-names 'symbols))

;(setq giac-builtins-regexp (myC  giac-functions-names ) )


(setq giac-builtins-regexp (mapconcat 'identity giac-functions-names "(\\|"))
(defconst giac-font-lock-keywords-1 (c-lang-const c-matchers-1 giac)
  "Minimal highlighting for giac mode.")

(defconst giac-font-lock-keywords-2 (c-lang-const c-matchers-2 giac)
  "Fast normal highlighting for giac mode.")

(defconst giac-font-lock-keywords-3 (c-lang-const c-matchers-3 giac)
  "Accurate normal highlighting for giac mode.")

(defvar giac-font-lock-keywords giac-font-lock-keywords-2
  "Default expressions to highlight in giac mode.")



   (defun giac-font-lock-keywords-2 ()
     (c-compose-keywords-list giac-font-lock-keywords-2))
   (defun giac-font-lock-keywords-3 ()
     (c-compose-keywords-list giac-font-lock-keywords-3))
   (defun giac-font-lock-keywords ()
     (c-compose-keywords-list giac-font-lock-keywords))


;; (defvar giac-mode-syntax-table nil
;;   "Syntax table used in giac mode.")

;; (message "Setting giac-mode-syntax-table to nil to force re-initialization")
;; (setq giac-mode-syntax-table nil)

(defvar giac-mode-syntax-table
    (setq giac-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table giac))))

(c-add-style "giac"
                  '("GNU"
                    (c-basic-offset . 4)
                    (c-comment-only-line-offset . (0 . 0))
                    (c-offsets-alist . ((inline-open           . 0)
                                        (arglist-intro         . +)
                                        (arglist-close         . 0)
                                        (inexpr-class          . 0)
                                        (case-label            . +)
                                        (cpp-macro             . c-lineup-dont-change)
                                        (substatement-open     . 0)))))
     
     (eval-and-compile
       (unless (or (stringp c-default-style)
                   (assoc 'giac-mode c-default-style))
         (setq c-default-style
               (cons '(giac-mode . "giac")
                     c-default-style))))
     



(defvar giac-mode-map (let ((map (c-make-inherited-keymap)))
		      ;; Add bindings which are only useful for GIAC
		      map)
  "Keymap used in giac-mode buffers.")





(c-define-abbrev-table 'giac-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gac" . giac-mode))
  
(define-derived-mode giac-mode prog-mode "giac"
  "Major mode for giac"

  :after-hook (c-update-modeline)
  :syntax-table c-mode-syntax-table
  (c-initialize-cc-mode t)
  (c-init-language-vars giac-mode)
  (c-common-init 'giac-mode)
  (c-set-style "linux")
  (setq indent-tabs-mode t)
   (setq tab-width 4)
  (c-toggle-auto-newline -1)
  (c-toggle-hungry-state -1)
  (run-hooks 'c-mode-common-hook)
  (run-mode-hooks 'c-mode-common-hook)

  ;; (font-lock-add-keywords 'giac-mode
  ;; 			  '(("cos\\|sin\\|tan\\|cosh\\|sinh\\|exp\\|ln\\|log\\|tanh\\|atan\\|acos\\|asin\\|sqrt\\|acosh\\|asinh\\|atanh\\|floor\\|ceil" . font-lock-function-name-face))
  ;; 			  )

    (font-lock-add-keywords 'giac-mode
			  `((,giac-builtins-regexp . font-lock-function-name-face))
			  )
;; (font-lock-add-keywords 'giac-mode

;; 			  '( giac-builtins-regexp  0  'show-paren-mismatch-face)
;; 			  )

)

(provide 'giac-mode)
