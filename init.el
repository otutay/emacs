;; add name and mail
;; code
(setq user-full-name "osmant")
(setq user-mail-address "otutaysalgir@gmail.com")

;; ask y-n instead of yes-no
(fset 'yes-or-no-p 'y-or-n-p)
;; show corresponding parenthesis
(show-paren-mode t)

;;highlight tabulations
(setq-default highlight-tabs t)

;; show trailing white spaces
(setq-default show-trailing-whitespace t)

;; recent file list
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; remove useless whitespace before saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; copy line below
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y\C-p")


;; Set locale to UTF8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Add package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))


(setq pmoc "package.el")

;; List of all wanted packages
(setq
 wanted-packages
 '(
   switch-window
   multiple-cursors
   magit
   dracula-theme
   ido-hacks
   ido-vertical-mode
   highlight-symbol
   flycheck
   auto-complete
   autopair
   solarized-theme
   autopair
))

;; Package manager and packages handler
(defun install-wanted-packages ()
  "Install wanted packages according to a specific package manager"
  (interactive)
  (cond
   ;; package.el
   ((string= pmoc "package.el")
    (require 'package)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
    (package-initialize)
    (let ((need-refresh nil))
      (mapc (lambda (package-name)
	  (unless (package-installed-p package-name)
	(set 'need-refresh t))) wanted-packages)
      (if need-refresh
	(package-refresh-contents)))
    (mapc (lambda (package-name)
	(unless (package-installed-p package-name)
	  (package-install package-name))) wanted-packages)
    )
   ;; el-get
   ((string= pmoc "el-get")
    (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
    (unless (require 'el-get nil 'noerror)
      (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
    (el-get 'sync wanted-packages))
   ;; fallback
   (t (error "Unsupported package manager")))
  )

;; Install wanted packages
(install-wanted-packages)

;load solarized theme
;;(load-theme 'solarized-dark t)
(load-theme 'dracula t)


;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
		       (if (not (minibufferp (current-buffer)))
			 (auto-complete-mode 1))
		       ))
(real-global-auto-complete-mode t)


;; autopair open in all buffer
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers


;; flycheck en
(global-flycheck-mode)

;; highlight set codes
(require 'highlight-symbol)

(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; set ido mode
(require 'ido)
(ido-mode t)

;; enable line number
(global-linum-mode t)

;; magit enable

;; multiple cursor enable and modify it
(require 'multiple-cursors)

(global-set-key (kbd "C-c C-a e l") 'mc/edit-lines)
(global-set-key (kbd "C-c C-a m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-a m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a m a") 'mc/mark-all-like-this)

;; switch window
;(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)



;; shell shortcut
(global-set-key [f1] 'shell)

;; eval buffer
(global-set-key [f5] 'eval-buffer)


;; ghdl setup

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode t))

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode t)
  :config
  ;; Company Flx adds fuzzy matching to company, powered by the sophisticated
  ;; sorting heuristics  in =flx=
  (use-package company-flx
    :ensure t
    :after company
    :init (company-flx-mode t))
  ;; Company Quickhelp
  ;; When idling on a completion candidate the documentation for the
  ;; candidate will pop up after `company-quickhelp-delay' seconds.
  (use-package company-quickhelp
    :after company
    :ensure t
    ;; :init (company-quickhelp-mode t)
    :hook (prog-mode . (lambda ()
			 (when (window-system)
			   (company-quickhelp-local-mode))))
    :config
    (setq company-quickhelp-delay 0.2
	  company-quickhelp-max-lines nil)))



(use-package lsp-mode
  :defer t
  :ensure t
  :commands lsp
  :config
  (setq lsp-log-io nil
	lsp-auto-configure t
	lsp-auto-guess-root t
	lsp-enable-completion-at-point t
	lsp-enable-xref t
	lsp-prefer-flymake nil
	lsp-use-native-json t
	lsp-enable-indentation t
	lsp-response-timeout 10
	lsp-restart 'auto-restart
	lsp-keep-workspace-alive t
	lsp-eldoc-render-all nil
	lsp-enable-snippet nil
	lsp-enable-folding t)
   ;;; lsp-ui gives us the blue documentation boxes and the sidebar info
  (use-package lsp-ui
    :defer t
    :ensure t
    :after lsp
    :commands lsp-ui-mode
    :config
    (setq lsp-ui-sideline-ignore-duplicate t
	  lsp-ui-sideline-delay 0.5
	  lsp-ui-sideline-show-symbol t
	  lsp-ui-sideline-show-hover t
	  lsp-ui-sideline-show-diagnostics t
	  lsp-ui-sideline-show-code-actions t
	  lsp-ui-peek-always-show t
	  lsp-ui-doc-use-childframe t)
    :bind
    (:map lsp-ui-mode-map
	  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	  ([remap xref-find-references] . lsp-ui-peek-find-references))
    :hook
    ((lsp-mode . lsp-ui-mode)
     (lsp-after-open . (lambda ()
			 (lsp-ui-flycheck-enable t)
			 (lsp-ui-sideline-enable t)
			 (lsp-ui-imenu-enable t)
			 (lsp-lens-mode t)
			 (lsp-ui-peek-enable t)
			 (lsp-ui-doc-enable t)))))
  ;;; company lsp
  ;; install LSP company backend for LSP-driven completion
  (use-package company-lsp
    :defer t
    :ensure t
    :after company
    :commands company-lsp
    :config
    (setq company-lsp-cache-candidates t
	  company-lsp-enable-recompletion t
	  company-lsp-enable-snippet t
	  company-lsp-async t)
    ;; avoid, as this changes it globally do it in the major mode instead (push
    ;; 'company-lsp company-backends) better set it locally
    :hook (lsp-after-open . (lambda()
			      (add-to-list (make-local-variable 'company-backends)
					   'company-lsp)))))
(use-package vhdl-mode
  :defer t
  :config
  (require 'lsp)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("ghdl-ls" "-v" "--trace-file=/home/otutay/.emacs.d/vhdl-ls.trace"))
		    :major-modes '(vhdl-mode)
		    :priority -1
		    :server-id 'lsp-vhdl-mode))
  :hook (vhdl-mode . (lambda()
		       (lsp)
		       (flycheck-mode t)
		       (add-to-list 'lsp-language-id-configuration '(vhdl-mode . "vhdl")))))
;; cmake mode
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(autoload 'cmake-mode "~/.emacs.d/cmake-mode.el" t)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-ghdl-workdir
   "C:\\Users\\otutaysalgir\\Desktop\\osmant\\Codes\\Projects\\DDSWithEth\\srcRTL")
 '(package-selected-packages
   (quote
    (company-lsp lsp-ui company-quickhelp company-flx use-package lsp-mode switch-window multiple-cursors magit dracula-theme ido-vertical-mode ido-hacks highlight-symbol flycheck solarized-theme autopair auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
