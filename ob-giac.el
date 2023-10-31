;;; ob-template.el --- org-babel functions for template evaluation

;; Copyright (C) your name here

;; Author: Vincek
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.02

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Requirements: giac, linux?? 

;; Use this section to list the requirements of this language.  Most
;; languages will require that at least the language be installed on
;; the user's system, and the Emacs major mode relevant to the
;; language be installed as well.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 's)
(require 'giac-mode)

(defgroup ob-giac nil
  "Options de configuration d'ob-giac"
  :group 'babel)
(defcustom giac-program "/usr/local/bin/giac"
  "Command to invoke giac"
  :group 'ob-giac
  :type 'string)


;; possibly require modes required for your language






;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("giac" . "gac"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:giac '())


(setq comint-prompt-regexp "[0-9]+>>")

(defvar org-babel-giac-eoe "/* Stop */"
 "String to indicate that evaluation has completed.")


;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:giac' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:giac (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-giac nil t)
  (let (
	(vars (org-babel--get-vars (or processed-params (org-babel-process-params params))))
	(latex (cdr (assq :latex processed-params)))
	)
    ;; (setq-local maliste vars)
    ;; (while maliste
    ;;   (message (format "%s::: %s" (car (car maliste)) (cdr (car maliste)))
    ;;     (setq maliste (cdr maliste)))
    ;;   )
    
    ;; Remove empty lines
    (setf body (replace-regexp-in-string "^[:empty:]*\n" "" body))
    ;; Remove newline in function body
	 ;(setf full-body (replace-regexp-in-string ";[[:blank:]]*\n" "; " full-body))
	 ;(setf full-body (replace-regexp-in-string "{[[:blank:]]*\n" "{ " full-body))
	 ;(setf full-body (replace-regexp-in-string "}[[:blank:]]*\n" "} " full-body))
	                                                    
    (setf body (replace-regexp-in-string "\\(;\\|{\\|}\\)[[:blank:]]*\n" "\\1 " body))
    (message latex)
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s:=%s"
                (car pair) (org-babel-giac-elisp-to-giac (cdr pair))))
      vars)
      "\n"
      body "\n" (if (string= latex "t") "latex(ans(-1))"))
    )
  )

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.


(defun org-babel-execute:giac (body params)
  "Execute a block of Giac code with org-babel.
This function is called by `org-babel-execute-src-block'"
  ;(message "Executing Giac source code block")
  
  ;(message "body: %s " body)
 
       (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the value of the session keyword is not the
         ;; string `none'

	      
         ;; (session (unless (string= "session" "none")
	 ;; 	    (org-babel-giac-initiate-session
         ;;             (cdr (assq :session processed-params)))))
	 (session   (org-babel-giac-initiate-session "*giac*"))
	      
         ;; variables assigned for use in the block
         (vars (org-babel--get-vars processed-params))
         (result-params (assq :result-params processed-params))
	 (latex (cdr (assq :latex processed-params)))
	 ;; either OUTPUT or VALUE which should behave as described above
         (result-type (assq :result-type processed-params))

	 ;; expand the body with `org-babel-expand-body:giac'
         (full-body (org-babel-expand-body:giac
                     body params processed-params))
					; (delete-blank-lines full-body)
	 (sortie-brute  (org-babel-comint-with-output
				(session (format "%s" org-babel-giac-eoe)  full-body)
			      (dolist (code (list full-body ))
				(insert (org-babel-chomp code))
;	(message "input: %s" (org-babel-chomp code))
				(comint-send-input nil t))
			      (insert (format "%s" org-babel-giac-eoe) )
      			      (comint-send-input nil )	)))
	 
	 (setq giac-last-output  (car
				  (split-string
				   (car
				    (last sortie-brute 3)
				    ) "\n")
				  ))
	 (string-match ".*\"\\(.*?\\)\""  giac-last-output  )
	 (if (string= latex "t") (concat "\\(" (match-string 1 giac-last-output ) "\\)") giac-last-output)
	 )
	 
       

       )

;; This function should be used to assign any variables in params in
;; the context of the session environment.

(defun org-babel-prep-session:giac (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-giac-initiate-session session))
	 (vars (list "maple\_mode(0)\n"))
	  )
   ; (comint-send-string session "maple\_mode(0)\n")
    (when session
     
      (org-babel-comint-in-buffer session
	(mapc (lambda (var)
		(end-of-line 1)
		(insert (org-babel-chomp   var))
		(comint-send-input nil t)
	      )
	    vars))
    
    session)
    )
  )



(defun org-babel-giac-var-to-giac (pair)
  "Convert an elisp var into a string of giac source code
specifying a var of the same value."
  (message "%S" pair)
  (let ((var (car pair))
        (val (cdr pair)))
    (message "%s = %s" var val)
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
        (setq val (string-to-char val))))
    (format "%S:= %s$" var
	    (org-babel-giac-elisp-to-giac val)))
  )

(defun org-babel-giac-elisp-to-giac (val)
  "Return a string of giac code which evaluates to VAL."
  
  (if (listp val)
      (concat "[" (mapconcat #'org-babel-giac-elisp-to-giac val ", ") "]")
    (format "%s; " val)))



(defun org-babel-giac-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-giac-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")

    (let ((session (or session "*giac*")))

      (apply 'make-comint "giac" giac-program nil)
      
      )
    
    ))

;; (defvar giac-font-lock-keywords
;;   (list
;;    ;; highlight all the reserved commands.
;;    `(,(concat "\\_<" (regexp-opt giac-keywords) "\\_>") . font-lock-keyword-face))
;;   "Additional expressions to highlight in `giac-mode'.")

(defun giac-dynamic-completion-function ()
  (when-let* ((bds (bounds-of-thing-at-point 'symbol))
              (beg (car bds))
              (end (cdr bds)))
    (when (> end beg)
      (list beg end giac-keywords :annotation-function (lambda (_) "giac-keywords")))))

(define-derived-mode giac-comint-mode comint-mode "giac comint mode"
  ;; How to display the process status in the mode-line
  (setq mode-line-process '(":%s"))
  ;; This disables editing and traversing the "=>" prompts
  (setq-local comint-prompt-read-only t)
  ;; Lets comint mode recognize the prompt
  (setq-local comint-prompt-regexp "[0-9]+>>")


  ;;
  (setq-local comint-input-history-ignore "^//")
  (add-hook 'comint-dynamic-complete-functions
            #'giac-dynamic-completion-function nil 'local)
  (make-local-variable 'company-backends)
  (cl-pushnew 'company-capf company-backends))



(defun giac-end-of-ouput (string)
  "Return non-nil si le prompt est vide"
  (s-matches? comint-prompt-regexp string )
  )

(provide 'ob-giac)
;;; ob-giac.el ends here
