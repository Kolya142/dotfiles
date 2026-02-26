(require 'package)
(setq package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ))

(package-initialize)

(defvar straight_el_bootstrap-version)
(let ((straight_el_bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (straight_el_bootstrap-version 5))
  (unless (file-exists-p straight_el_bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
     "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load straight_el_bootstrap-file nil 'nomessage))

(straight-use-package 'magit)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/ThirdParty/lean4-mode")
(require 'lean4-mode)

(add-to-list 'load-path "~/.emacs.d/theme69-theme.el")

(package-install 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)

(package-install 'company)
(require 'company)

(package-install 'multiple-cursors)
(require 'multiple-cursors)

(global-set-key (kbd "C-c C-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this)

(global-company-mode)

;; (require 'ido-completing-read+)
;; (ido-mode 1)
;; (ido-everywhere 1)

(use-package rainbow-delimiters
  :ensure t
)

(package-install 'dash)
(package-install 'lsp-mode)

(package-install 'avy)

(package-install 'compat)
(package-install 'llama)

(package-install 'fixmee)

(require 'fixmee)
(package-install 'button-lock)
(require 'button-lock)
(global-fixmee-mode 1)

(package-install 'julia-mode)
(require 'julia-mode)

(global-set-key (kbd "C-:") 'avy-goto-char)

(global-set-key (kbd "M-p 0") #'dictionary-lookup-definition)

(line-number-mode)
(global-display-line-numbers-mode)

(electric-indent-mode 0)

; (load-theme 'theme69 'NO-CONFIRM)
(load-theme 'tango-dark)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

(package-install 'scala-mode)
(package-install 'd-mode)
(package-install 'yaml-mode)
(package-install 'glsl-mode)
(package-install 'tuareg)
(package-install 'lua-mode)
(package-install 'less-css-mode)
(package-install 'graphviz-dot-mode)
(package-install 'clojure-mode)
(package-install 'cmake-mode)
(package-install 'rust-mode)
(package-install 'csharp-mode)
(package-install 'cc-mode)
(package-install 'nim-mode)
(package-install 'jinja2-mode)
(package-install 'forth-mode)
(package-install 'markdown-mode)
(package-install 'purescript-mode)
(package-install 'nix-mode)
(package-install 'dockerfile-mode)
(package-install 'toml-mode)
(package-install 'nginx-mode)
(package-install 'kotlin-mode)
(package-install 'go-mode)
(package-install 'php-mode)
(package-install 'racket-mode)
(package-install 'qml-mode)
(package-install 'ag)
(package-install 'elpy)
(package-install 'typescript-mode)
(package-install 'rfc-mode)
(package-install 'sml-mode)

(setq make-backup-files nil)
(setq auto-save-default nil)


(add-to-list 'auto-mode-alist '("\\.faivy\\'" . jai-mode))

(defun base-mode-font-lock-keywords ()
  (list
   '( "\\b[-+]?0?(xbo)?[0-9]+" . font-lock-constant-face)
   `(,(regexp-opt (list "^" "&" "!" "=" "$" "#" "&&" "||" "+" "-" "*" "/" "." "=>" "<" ">" "<=" ">=" "%" "~" "?")) . font-lock-keyword-face)
   '( ";; .*" . font-lock-comment-face)
   '( "//.*" . font-lock-comment-face)
   '( "# .*" . font-lock-comment-face)
  )
)
(defun base-mode-indent-line ()
  (interactive)
  (let ((indent (save-excursion
      (forward-line -1)
      (while (and (not (bobp))
                  (string-empty-p
                   (string-trim-right
                    (thing-at-point 'line t))))
        (forward-line -1))
      (current-indentation)
    )))
    (if (or (> (current-indentation) (+ indent 4)) (string-empty-p (string-trim-right (thing-at-point 'line t))))
      (indent-line-to indent)
    )
  )
)

(define-derived-mode base-mode prog-mode "base-mode"
  "base-mode. based on simpc-mode"
  (setq-local font-lock-defaults (list (base-mode-font-lock-keywords)))
  (setq-local indent-line-function 'base-mode-indent-line)
)
(defun base-mode-rainbow ()
  (rainbow-delimiters-mode 1)
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#FFD0D0"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#800080"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#D010D0"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#FF8080"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#5080F0"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#805080"))))
  )
)
(add-hook 'base-mode-hook #'base-mode-rainbow)

(defun init/reload ()
  "Reload init.el configuration file."
  (interactive)
  (load-file "~/.emacs.d/init.el")
)

(add-to-list 'auto-mode-alist '("\\.lisp\\'" . base-mode))

;; (load-file "~/.emacs.d/fasm-mode.el")


(require 'subr-x)

;; simpc by Tsoding
(defvar simpc-mode-syntax-table
  (let ((table (make-syntax-table)))
	(modify-syntax-entry ?/ ". 124b" table)
	(modify-syntax-entry ?* ". 23" table)
	(modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)

    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?% "." table)
    table))

(defun simpc-types ()
  '("char" "int" "long" "short" "void" "bool" "float" "double" "signed" "unsigned"
    "char16_t" "char32_t" "char8_t"
    "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t" "int64_t" "uint64_t"
    "uintptr_t"
    "size_t"))

(defun simpc-keywords ()
  '("auto" "break" "case" "const" "continue" "default" "do"
    "else" "enum" "extern" "for" "goto" "if" "register"
    "return"  "sizeof" "static" "struct" "switch" "typedef"
    "union"  "volatile" "while" "alignas" "alignof" "and"
    "and_eq" "asm" "atomic_cancel" "atomic_commit" "atomic_noexcept" "bitand"
    "bitor" "catch"  "class" "co_await"
    "co_return" "co_yield" "compl" "concept" "const_cast" "consteval" "constexpr"
    "constinit" "decltype" "delete" "dynamic_cast" "explicit" "export" "false"
    "friend" "inline" "mutable" "namespace" "new" "noexcept" "not" "not_eq"
    "nullptr" "operator" "or" "or_eq" "private" "protected" "public" "reflexpr"
    "reinterpret_cast" "requires" "static_assert" "static_cast" "synchronized"
    "template" "this" "thread_local" "throw" "true" "try" "typeid" "typename"
    "using" "virtual" "wchar_t" "xor" "xor_eq"))

(defun simpc-font-lock-keywords ()
  (list
   `("# *[#a-zA-Z0-9_]+" . font-lock-preprocessor-face)
   `("#.*include \\(\\(<\\|\"\\).*\\(>\\|\"\\)\\)" . (1 font-lock-string-face))
   `(,(regexp-opt (simpc-keywords) 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt (simpc-types) 'symbols) . font-lock-type-face)))

(defun simpc--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun simpc--indentation-of-previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (current-indentation)))

(defun simpc--desired-indentation ()
  (let* ((cur-line (string-trim-right (thing-at-point 'line t)))
         (prev-line (string-trim-right (simpc--previous-non-empty-line)))
         (indent-len 4)
         (prev-indent (simpc--indentation-of-previous-non-empty-line)))
    (cond
     ((string-match-p "^\\s-*switch\\s-*(.+)" prev-line)
      prev-indent)
     ((and (string-suffix-p "{" prev-line)
           (string-prefix-p "}" (string-trim-left cur-line)))
      prev-indent)
     ((string-suffix-p "{" prev-line)
      (+ prev-indent indent-len))
     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- prev-indent indent-len) 0))
     ((string-suffix-p ":" prev-line)
      (if (string-suffix-p ":" cur-line)
          prev-indent
        (+ prev-indent indent-len)))
     ((string-suffix-p ":" cur-line)
      (max (- prev-indent indent-len) 0))
     (t prev-indent))))

(defun simpc-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation
            (simpc--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

(define-derived-mode simpc-mode prog-mode "Simple C"
  "Simple major mode for editing C files."
  :syntax-table simpc-mode-syntax-table
  (setq-local font-lock-defaults '(simpc-font-lock-keywords))
  (setq-local indent-line-function 'simpc-indent-line)
  (setq-local comment-start "// "))

(add-to-list 'auto-mode-alist '("\\.c\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . simpc-mode))


(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode))  ;;

;; https://github.com/rexim/dotfiles/blob/master/.emacs
(defun setup-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'tuareg-mode-hook 'setup-whitespace-handling)
(add-hook 'c++-mode-hook 'setup-whitespace-handling)
(add-hook 'c-mode-hook 'setup-whitespace-handling)
(add-hook 'simpc-mode-hook 'setup-whitespace-handling)
(add-hook 'emacs-lisp-mode 'setup-whitespace-handling)
(add-hook 'java-mode-hook 'setup-whitespace-handling)
(add-hook 'lua-mode-hook 'setup-whitespace-handling)
(add-hook 'rust-mode-hook 'setup-whitespace-handling)
(add-hook 'scala-mode-hook 'setup-whitespace-handling)
(add-hook 'markdown-mode-hook 'setup-whitespace-handling)
(add-hook 'haskell-mode-hook 'setup-whitespace-handling)
(add-hook 'python-mode-hook 'setup-whitespace-handling)
(add-hook 'erlang-mode-hook 'setup-whitespace-handling)
(add-hook 'asm-mode-hook 'setup-whitespace-handling)
(add-hook 'fasm-mode-hook 'setup-whitespace-handling)
(add-hook 'go-mode-hook 'setup-whitespace-handling)
(add-hook 'nim-mode-hook 'setup-whitespace-handling)
(add-hook 'yaml-mode-hook 'setup-whitespace-handling)
(add-hook 'porth-mode-hook 'setup-whitespace-handling)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(set-frame-font "Iosevka Term-11")



;; https://stackoverflow.com/questions/15946178/
(setq whitespace-display-mappings
  '(
    (space-mark 32 [183] [46])
    (tab-mark 9 [9655 9] [92 9])
    ))
