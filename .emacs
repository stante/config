;;                                                 
;;      █████  ██████████   ██████    █████   ██████
;;     ██░░░██░░██░░██░░██ ░░░░░░██  ██░░░██ ██░░░░ 
;;    ░███████ ░██ ░██ ░██  ███████ ░██  ░░ ░░█████ 
;;  ██░██░░░░  ░██ ░██ ░██ ██░░░░██ ░██   ██ ░░░░░██
;; ░██░░██████ ███ ░██ ░██░░████████░░█████  ██████ 
;; ░░  ░░░░░░ ░░░  ░░  ░░  ░░░░░░░░  ░░░░░  ░░░░░░  
;; Alexander Stante's .emacs file

;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; MELPA
;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("melpa-sta" . "http://melpa.org/packages/"))
(package-initialize)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/home/alex/.emacs.d/bookmarks")
 '(custom-enabled-themes '(doom-dracula))
 '(custom-safe-themes
   '("e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "d1c7f2db070c96aa674f1d61403b4da1fff2154163e9be76ce51824ed5ca709c" "e456955baadeba1eae3f32bf1dc65a2c69a561a714aae84e3278e1663454fe31" default))
 '(package-selected-packages
   '(counsel swiper yaml-mode doom-themes ein counsel-projectile ivy auto-complete markdown-mode realgud-ipdb realgud elpy yasnippet w3m undo-tree slime sauron projectile paredit markdown-mode+ magit load-theme-buffer-local iy-go-to-char iedit ido-hacks highlight-parentheses helm glsl-mode flycheck bookmark+ autopair auto-complete-clang auctex arduino-mode all)))


;; ---------------------------------------------------------------------------
;; Basic configuration
;; ---------------------------------------------------------------------------

;; disable menu bar
(menu-bar-mode -1)

;; disable toolbar
(tool-bar-mode -1)

;; disable scrollbar
(scroll-bar-mode -1)

;; delete whole selection when character is entered
(delete-selection-mode 1)

;; set font
(set-face-attribute 'default nil :family 'mononoki' :height 110)

;; disable startup screen
(setq inhibit-splash-screen t)

;; default directory
(setq default-directory "~/")

;; show recent files in menu
(recentf-mode)

;; disabling backup files
(setq backup-inhibited t)

;; disable autosave
(setq auto-save-default nil)

;; don't put cursor in the middle of the screen after 
(setq scroll-conservatively 1)

;; set default tab width to 4
(setq default-tab-width 4)

;; no audio bell
(setq visible-bell t)

;; focus-follows-mouse in windows
(setq mouse-autoselect-window t)

;; write time stamp to file when saving
(add-hook 'before-save-hook 'time-stamp)



;; ---------------------------------------------------------------------------
;; Own functions
;; ---------------------------------------------------------------------------

;; scroll screen without moving cursor
(defun my-scroll-up-line ()
  "Scroll screen one line up."
  (interactive)
  (scroll-up 1)
  (next-line))
(global-set-key (kbd "C-S-n") 'my-scroll-up-line)

(defun my-scroll-down-line ()
  "Scroll screen one line down."
  (interactive)
  (scroll-down 1)
  (previous-line))
(global-set-key (kbd "C-S-p") 'my-scroll-down-line)


;; command similar to vim o and O
(defun my-open-line-above ()
  "Opens a line above cursor."
  (interactive)
  (move-beginning-of-line 1)
  (newline-and-indent)
  (previous-line))
(global-set-key (kbd "C-S-o") 'my-open-line-above)

(defun my-open-line-below ()
  "Opens a line below the cursor."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "C-o") 'my-open-line-below)

;; toggle with M-<backspace> the active window
;;(global-set-key (kbd "M-<backspace>") 'other-window)
;;(global-set-key (kbd "M-S-<backspace>") '(other-window -1))


(defun other-window-backward (&optional n)
  "Moves cursor one window backwards"
  (interactive "p")
  (other-window (- (or n 1))))


(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") 'other-window-backward)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-^") 'enlarge-window)
(global-set-key (kbd "C-&") 'shrink-window)

(windmove-default-keybindings)

;; dbus
(defun stante-dbus-capable ()
  "Check if dbus is available"
  (unwind-protect
      (let (retval)
        (condition-case ex
            (setq retval (dbus-ping :session "org.freedesktop.Notifications"))
          ('error
           (message (format "Error: %s - No dbus" ex))))
        retval)))

(defun stante-kde-notification (summary body icon timeout)
  "call notification-daemon method METHOD with ARGS over dbus"
  (if (stante-dbus-capable)
      (dbus-call-method
       :session                                 ; Session (not system) bus
       "org.freedesktop.Notifications"          ; Service name
       "/org/freedesktop/Notifications"         ; Service path
       "org.freedesktop.Notifications" "Notify" ; Method
       "emacs-visual-notifications"
       0
       icon
       summary
       body
       '(:array)
       '(:array :signature "{sv}")
       ':int32 timeout)
    (message "Oh well, you're still notified")))

;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; Shipped extensions
;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; emacs-lisp-mode
;; ---------------------------------------------------------------------------
;; (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
;; (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)


;; ---------------------------------------------------------------------------
;; org-mode
;; ---------------------------------------------------------------------------
(require 'org-install)
(require 'remember)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

; Standard key bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)


; Set location of org files on local system
(setq org-directory "~/org/")

; Set location of the notes file
(setq org-default-notes-file (concat org-directory "notes.org"))

; Set files for agenda
(setq org-agenda-files (list org-default-notes-file))

;; ---------------------------------------------------------------------------
;; linum-mode
;; ---------------------------------------------------------------------------
(setq linum-format "%d ")

;; ---------------------------------------------------------------------------
;; CC-Mode
;; ---------------------------------------------------------------------------
(defun my-c-initialization-hook ()
  (define-key c-mode-base-map [remap c-end-of-defun] '(lambda () 
							(interactive) 
							(c-beginning-of-defun -1))))
(add-hook 'c-initialization-hook 'my-c-initialization-hook)
(add-hook 'c-initialization-hook 'linum-mode)
; (add-hook 'c++-initialization-hook 'linum-mode)

(setq c-default-style '((java-mode . "java")
						(awk-mode . "awk")
						(other . "linux")))
(setq c-basic-offset 4)

(defun my-c++-mode-hook ()
  (setq c-basic-offset4)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'innamespace 0)
  (linum-mode))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook)


(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

;; ---------------------------------------------------------------------------
;; Winner-mode
;; ---------------------------------------------------------------------------
(winner-mode 1)

;; ---------------------------------------------------------------------------
;; CMake
;; ---------------------------------------------------------------------------
(require 'cmake-mode)
(setq auto-mode-alist
	  (append '(("CMakeLists\\.txt\\'" . cmake-mode)
				("\\.cmake\\'" . cmake-mode))
				auto-mode-alist))
 
;; ---------------------------------------------------------------------------
;; Rcirc
;; ---------------------------------------------------------------------------
(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
	 "Reconnect the server process."
	 (interactive "i")
	 (unless process
	   (error "There's no process for this target"))
	 (let* ((server (car (process-contact process)))
			(port (process-contact process :service))
			(nick (rcirc-nick process))
			channels query-buffers)
	   (dolist (buf (buffer-list))
		 (with-current-buffer buf
		   (when (eq process (rcirc-buffer-process))
			 (remove-hook 'change-major-mode-hook ;
						  'rcirc-change-major-mode-hook)
			 (if (rcirc-channel-p rcirc-target)
				 (setq channels (cons rcirc-target channels))
			   (setq query-buffers (cons buf query-buffers))))))
	   (delete-process process)
	   (rcirc-connect server port nick
					  rcirc-default-user-name
					  rcirc-default-user-full-name
					  channels))))

(add-hook 'rcirc-mode-hook 'rcirc-track-minor-mode)

;; ---------------------------------------------------------------------------
;; Rcirc
;; ---------------------------------------------------------------------------
;; <return> bug fix

;; ---------------------------------------------------------------------------
;; Movement
;; ---------------------------------------------------------------------------
(defun forward-word-to-beginning (&optional n)
  "Move point forward n word and place cursor at the beginning."
  (interactive "p")
  (let (myword)
	(setq myword
		  (if (and transient-mark-mode mark-active)
			  (buffer-substring-no-properties (region-beginning) (region-end))
			(thing-at-point 'symbol)))
	(if (not (eq myword nil))
		(forward-word n))
	(forward-word n)
	(backward-word n)))

(global-set-key (kbd "M-f") 'forward-word-to-beginning)


;; ---------------------------------------------------------------------------
;; CEDET Mode
;; ---------------------------------------------------------------------------

;; (global-ede-mode 1)
;; (semantic-load-enable-code-helpers)

;; ---------------------------------------------------------------------------
;; eldoc-mode
;; ---------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook 
		  '(lambda () (turn-on-eldoc-mode)))


;; ---------------------------------------------------------------------------
;; ido-mode
;; ---------------------------------------------------------------------------
;; (require 'ido)
;; (ido-mode t)
;; (setq ido-enable-flex-matching t)

;; ---------------------------------------------------------------------------
;; eshell
;; ---------------------------------------------------------------------------
(setq eshell-prompt-function
	  (lambda () (if (= (user-uid) 0) "# " "$ ")))

(setq eshell-prompt-regexp "[#$] ")

;; ---------------------------------------------------------------------------
;; ibuffer
;; ---------------------------------------------------------------------------
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
	  (quote (("default"
			   ("Org"             ;; org-mode related buffers
				(mode . org-mode))
			   ("Mail"            ;; mu4e related buffers
				(or 
				 (mode . mu4e-view-mode)
				 (mode . mu4e-headers-mode)
				 (mode . mu4e-main-mode)
				 (mode . mu4e-compose-mode)
				 (name . "*mu4e-proc*")
				 (name . "*mu4e-raw-view*")))
			   ("Code"     ;; programming related buffers
				(or
				 (mode . perl-mode)
				 (mode . c-mode)
				 (mode . emacs-lisp-mode)
				 (mode . c++-mode)))
			   ("Dired"           ;; dired related buffers
				(mode . dired-mode))
			   ("Chat"             ;; IRC related buffers
				(mode . rcirc-mode))))))
(add-hook 'ibuffer-mode-hook
		  (lambda ()
			(ibuffer-switch-to-saved-filter-groups "default")))
(add-hook 'mu4e-view-mode-hook 'visual-line-mode)


;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; Host packages
;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; yasnippet
;; ---------------------------------------------------------------------------
(require 'yasnippet)
(yas/global-mode 1)
(setq yas-wrap-arount-region t)
;;(yas/load-directory "~/.emacs.d/elpa/yasnippet-20120822.52/snippets")


;; ---------------------------------------------------------------------------
;; glsl-mode
;; ---------------------------------------------------------------------------
(require 'glsl-mode)

;; ---------------------------------------------------------------------------
;; autopair
;; ---------------------------------------------------------------------------
;; (require 'autopair)
;; (autopair-global-mode)

; bug fix for return bug in ansi-term with autopair
(add-hook 'term-mode-hook
  #'(lambda () (setq autopair-dont-activate t)))

(add-hook 'ansi-term-hook
  #'(lambda () (setq autopair-dont-activate t)))

;; ---------------------------------------------------------------------------
;; auto-complete
;; ---------------------------------------------------------------------------
(require 'auto-complete-config)
(ac-config-default)

;;(setq ac-clang-flags
;;      (mapcar (lambda (item)(concat "-I" item))
;;              (split-string
;;                "
;;  /usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.0/../../../../include/c++/4.7.0
;;  /usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.0/../../../../include/c++/4.7.0/x86_64-unknown-linux-gnu
;;  /usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.0/../../../../include/c++/4.7.0/backward
;;  /usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.0/include
;;  /usr/local/include
;;  /usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.0/include-fixed
;;  /usr/include
;; "
;;                )))
;; 
(defun ac-c-mode-setup ()
  (setq ac-sources '(ac-source-semantic ac-source-semantic-raw ac-source-yasnippet)))
;  (setq ac-sources '(ac-source-gtags ac-source-abbrev ac-source-dictionary
;									 ac-source-words-in-same-mode-buffers)))
(add-hook 'c++-mode-hook 'ac-c-mode-setup)
(add-hook 'c-mode-hook 'ac-c-mode-setup)

;; ---------------------------------------------------------------------------
;; bbdb
;; ---------------------------------------------------------------------------
; (require 'bbdb)
; (bbdb-initialize 'gnus 'message)
; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;; ---------------------------------------------------------------------------
;; acutex
;; ---------------------------------------------------------------------------
; (load "auctex.el" nil t t)
; (load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'TeX-mode-hook 'reftex-mode)

;; ---------------------------------------------------------------------------
;; my-refactor
;; ---------------------------------------------------------------------------
(defun my-extract-method (beg end)
  "Refactor code by extracting to method."
  (interactive "r")
  (kill-region beg end)
  (end-of-defun)
  (yank))

;; ---------------------------------------------------------------------------
;; ediff
;; ---------------------------------------------------------------------------
(setq ediff-keep-variants nil)

;; ---------------------------------------------------------------------------
;; dired
;; ---------------------------------------------------------------------------
(setq ediff-keep-variants nil)
(setq dired-listing-switches "-lah")

;; ---------------------------------------------------------------------------
;; magit
;; ---------------------------------------------------------------------------
;; (global-set-key (kbd "C-c m s") 'magit-status)

;; ---------------------------------------------------------------------------
;; projectile
;; ---------------------------------------------------------------------------
(require 'projectile)
(projectile-global-mode)

;; ---------------------------------------------------------------------------
;; ivy
;; ---------------------------------------------------------------------------
(require 'ivy)
(ivy-mode 1)

;; ---------------------------------------------------------------------------
;; jedi
;; ---------------------------------------------------------------------------
;; (require 'jedi)
;; (add-to-list 'ac-sources 'ac-source-jedi-direct)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; ---------------------------------------------------------------------------
;; pp
;; ---------------------------------------------------------------------------
(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

;; ---------------------------------------------------------------------------
;; gud
;; ---------------------------------------------------------------------------
(defun stante-gud-toggle-breakpoint (&optional ARGS)
  (interactive)
  (if (not (gud-remove ARGS))
	  (gud-break ARGS)))

(add-hook 'gdb-mode-hook
		  #'(lambda () (global-set-key (kbd "<f10>") 'gud-next)))
(add-hook 'gdb-mode-hook
		  #'(lambda () (global-set-key (kbd "<f11>") 'gud-step)))
(add-hook 'gdb-mode-hook
		  #'(lambda () (global-set-key (kbd "<f9>") 'gud-break)))
(add-hook 'gdb-mode-hook
		  #'(lambda () (global-set-key (kbd "<f5>") 'gud-run)))
(add-hook 'gdb-mode-hook
		  #'(lambda () (global-set-key (kbd "C-<f10>") 'gud-until)))


;; ---------------------------------------------------------------------------
;; all
;; ---------------------------------------------------------------------------

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;; Hard to find variables etc. :)
(setq help-window-select t)
(put 'narrow-to-region 'disabled nil)

;; dirty temprorary stuff
;; (global-set-key
;;      "\M-x"
;;      (lambda ()
;;        (interactive)
;;        (call-interactively
;;         (intern
;;          (ido-completing-read
;;           "M-x "
;;           (all-completions "" obarray 'commandp))))))


;; undo tree mode
(global-undo-tree-mode)

;; ---------------------------------------------------------------------------
;; Custom keymap
;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c RET") 'bookmark-jump)
(global-set-key (kbd "C-c m") 'bookmark-set)
(global-set-key (kbd "C-c M") 'bookmark-delete)
(global-set-key (kbd "C-c .") 'find-file)
(global-set-key (kbd "C-c p p") 'counsel-projectile-switch-project)
(put 'dired-find-alternate-file 'disabled nil)
