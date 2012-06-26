;; see http://xahlee.org/emacs/elisp_comment_handling.html
;; see also http://xahlee.org/emacs/elisp_syntax_coloring.html for testing
;; Using font locks with maching: http://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html#Search_002dbased-Fontification
;; see also http://www.emacswiki.org/emacs/EmacsSyntaxTable for a list of syntax classes (e.g. for jumping over words, etc.)
;; for lists of name faces, see http://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html

(setq myKeywords
 `(
   ; The first matched rule wins

   ;; Indented things
   ;("^ +[^<>: ].*$" . font-lock-string-face)

   ;( ,(regexp-opt '("::" "<" ">" "=" "(" ")" "@") 'word) . font-lock-function-name-face)
   ;
   ( ,(regexp-opt '("::" ":" "<" ">" "<=" "=>") 'word) . font-lock-builtin-face)
   ( ,(regexp-opt '("action"
		    "baseline"
		    "branch"
		    "branchpoint"
		    "calls"
		    "config"
		    "func"
		    "group"
		    "global"
		    "package"
		    "plan"
		    "reach"
		    "submitter"
                    "summary"
		    "task"
		    "versioner"
		    "via"
		    "switch" "default") 'words) . font-lock-keyword-face)
   ( ,(regexp-opt '("checkout" "update" "local_version" "repo_version") 'words) . font-lock-keyword-face)
   ;( ,(regexp-opt '("Pi" "Infinity") 'word) . font-lock-constant-face)

   ;; see http://stackoverflow.com/questions/2970597/highlighting-correctly-in-an-emacs-major-mode
   ; stuff between "
   ("\"\\.\\*\\?" . font-lock-string-face)
   ("^\\[.*?\\]" . font-lock-function-name-face)

   ;(":\\|,\\|;\\|{\\|}\\|=>\\|@\\|$\\|=" . font-lock-keyword-face)
   ;( ,(regexp-opt mydsl-keywords 'words) . font-lock-builtin-face)
   ;( ,(regexp-opt mydsl-events 'words) . font-lock-constant-face)
  )
)

;; the command to comment/uncomment text
(defun ducttape-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
     (comment-dwim arg)))

;; define the major mode.
(define-derived-mode ducttape-mode fundamental-mode
"ducttape workflow"

  (setq font-lock-defaults '(myKeywords))

  ;; modify the keymap
  (define-key ducttape-mode-map [remap comment-dwim] 'ducttape-comment-dwim)

  ;; for more on modify-synatx-entry, see http://www.slac.stanford.edu/comp/unix/gnu-info/elisp_32.html
  ;; first char is class
  ;; second chart is reserved for ( and ) pairings (should be blank otherwise)
  ;; all following characters are synatx flags (e.g. a vs b style comments and 2-char seqs)

  ;; perl style comment: "# ..." 
  (modify-syntax-entry ?# "< b" ducttape-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" ducttape-mode-syntax-table)

  ;; c style comment “// …” 
  (modify-syntax-entry ?\/ ". 12b" ducttape-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" ducttape-mode-syntax-table)

  ;; Make { and } delimiters matching each other
  (modify-syntax-entry ?{ "(}")
  (modify-syntax-entry ?} "){")
)

(setq auto-mode-alist (cons '("\\.tape$" . ducttape-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tconf$" . ducttape-mode) auto-mode-alist))
