;;---------------------------------------------------------------------------
;;
;; Paths.
;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/scripts/")
(add-to-list 'load-path "~/.emacs.d/themes/")

;;---------------------------------------------------------------------------
;;
;; MELPA, sigh... Cut and paste boilerplate setup.
;; M-x package-list-packages to browse.
;;

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For "important compatibility libraries like cl-lib"
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
;; Just stable packages please
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;;---------------------------------------------------------------------------
;;
;; Backups, somewhere out of the way, please.
;;

(setq backup-directory-alist
      `((".*" . , "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/backups/" t)))

;;---------------------------------------------------------------------------
;;
;; Startup configuration.
;;

(server-start) ; Act as a server to emacsclient.
(setq inhibit-startup-screen t)
(setq initial-scratch-message
      ";; It is pitch black. You are likely to be eaten by a grue.\n\n")
(load-theme 'sanityinc-tomorrow-night t) ; Or zenburn.
(set-cursor-color "#f66")
(set-face-background 'mode-line "#413")
(set-face-background 'mode-line-inactive "#272727")
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(set-default-font "Consolas 16")
(tool-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(display-time-mode 1)
(setq display-time-default-load-average nil) ; Why, emacs.

;;---------------------------------------------------------------------------
;;
;; Startup folder.
;;

(setq default-directory "D:\\machine2\\oxpig\\oxc\\")

;;---------------------------------------------------------------------------
;;
;; Edit this file.
;;

(defun config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;---------------------------------------------------------------------------
;;
;; Margins.
;;

(setq-default left-margin-width 10 right-margin-width 0)
(set-window-buffer nil (current-buffer))

;;---------------------------------------------------------------------------
;;
;; Column numbering, and from 1, please.
;;

(setq column-number-mode 1)
(setq mode-line-position ; Warning: slow on large buffers!
      '("%p (%l," (:eval (format "%d)" (1+ (current-column))))))
(add-hook 'post-command-hook 'force-mode-line-update)

;;---------------------------------------------------------------------------
;;
;; Set the fill column and display its indicator.
;;

(setq-default fill-column 80)
(require 'fill-column-indicator)
(setq-default fci-rule-width 2)
(setq-default fci-rule-color "#505050")

;;---------------------------------------------------------------------------
;;
;; Pesky trailing whitespace.
;;

(require 'whitespace)
(setq-default whitespace-style '(face trailing tabs tab-mark lines-tail))
(setq-default whitespace-line-column 80)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'text-mode-hook 'whitespace-mode)

;;---------------------------------------------------------------------------
;;
;; Pesky tabs.
;;
(setq-default indent-tabs-mode nil)

;;---------------------------------------------------------------------------
;;
;; Multiple cursors.
;; Esc or C-g or return to bail; C-j for newline.
;;

(require 'multiple-cursors)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-l") 'mc/edit-lines)
(define-key mc/keymap [escape] 'mc/keyboard-quit)

;;---------------------------------------------------------------------------
;;
;; Global tweaks.
;;

(show-paren-mode 1)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "S-<f8>") 'previous-error)
(global-auto-revert-mode 1); Prompt me if it's modified, otherwise, go for it
(delete-selection-mode 1)

;;---------------------------------------------------------------------------
;;
;; C mode tweaks.
;;

(setq-default c-basic-offset 4)
(setq-default c-default-style "k&r")
(defun my-c-mode-hook ()
  (fci-mode 1))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;;---------------------------------------------------------------------------
;;
;; Org mode tweaks.
;;

(defun my-org-mode-hook ()
  (local-set-key (kbd "C-/") 'org-todo))
(add-hook 'org-mode-hook 'my-org-mode-hook)

;;---------------------------------------------------------------------------
;;
;; Git commit message "mode".
;;

(define-minor-mode git-commit-mode
  "Git commit mode"
  nil
  " Commit")
(add-to-list 'auto-mode-alist '(".*_EDITMSG\\'" . text-mode))
(defun my-git-commit-hook ()
  (if (string-match "_EDITMSG" buffer-file-name)
      (progn
        (git-commit-mode)
        (setq fill-column 72)
        (setq-local whitespace-line-column 50)
        (fci-mode 1))))
(add-hook 'text-mode-hook 'my-git-commit-hook)

;;---------------------------------------------------------------------------
;;
;; Compilation.
;;

(defun my-compile-command ()
  (interactive)
  (save-buffer)
  (compile "make -k "))

(require 'compile)
(add-to-list 'compilation-error-regexp-alist
             '("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)): \\(error\\|warning\\) C[0-9]+:" 1 3) t)

(setq compile-command "make -k ")
(setq compilation-read-command nil)
(setq compilation-scroll-output 'first-error) ; First error, or to the eeeeend...please.
(global-set-key (kbd "C-B") 'my-compile-command)

(defun my-rebuild-command ()
  (interactive)
  (save-buffer)
  (compile "make -k rebuild"))

(defun my-clean-build-command ()
  (interactive)
  (compile "make clean"))

(define-prefix-command 'my-build-commands)
(global-set-key (kbd "M-b") 'my-build-commands)

(global-set-key (kbd "M-b r") 'my-rebuild-command)
(global-set-key (kbd "M-b c") 'my-clean-build-command)

(defun my-compilation-window-setup ()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (split-window-below)
  (enlarge-window 11)
  (switch-to-buffer "*scratch*")
  (other-window 1)
  (switch-to-buffer "*compilation*")
  (other-window 1))

;;---------------------------------------------------------------------------
;;
;; Find matching braces.
;; Rather than navigating around the file randomly if off by one.
;;

(defun my-goto-matching-brace ()
  (interactive)
  (when (or (eq (char-before) ?{) (eq (char-before) ?\()) (backward-char))
  (if (or (eq (char-after) ?{) (eq (char-after) ?\())
      (forward-sexp)
    (progn
      (if (or (eq (char-before) ?}) (eq (char-before) ?\)))
          (progn
            (backward-sexp)
            (forward-char))
        (if (or (eq (char-after) ?}) (eq (char-after) ?\)))
            (progn
              (forward-char)
              (backward-sexp)))))))

(global-set-key (kbd "C-]") 'my-goto-matching-brace)

;;---------------------------------------------------------------------------
;;
;; CTags (go to definition).
;;

(setq path-to-ctags "ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format
    "%s --c-kinds=cdefglmnpstuvx -f TAGS -e -R %s"
    path-to-ctags (directory-file-name dir-name)))
  )

(global-set-key (kbd "M->") 'create-tags)

;;---------------------------------------------------------------------------
;;
;; Okay, look. Emacs shortcuts are actually pretty great, but then muscle memory
;; ruins one's ability to use any other software.
;;

(defun kill-region-or-bust()
  "kill the active region, or do nothing"
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (message "%s" "(No region selected, doing nothing)")))

(defun use-escape-to-cancel()
  "Remind the user that C-g does nothing"
  (interactive)
  (message "%s" "Press Escape to cancel. C-g does nothing."))

(defun kill-ring-save-or-bust()
  "Copy text, or do nothing"
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (message "%s" "(No region selected, doing nothing)")))

(defun my-kill-this-buffer()
  "Run kill-this-buffer or server-edit as appropriate"
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (kill-this-buffer)))

(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-y"))
(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "C-p"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "M-;"))

(define-prefix-command 'my-k-commands)
(global-set-key (kbd "C-k") 'my-k-commands)
(global-set-key (kbd "C-k C-c") 'comment-region)
(global-set-key (kbd "C-k C-u") 'uncomment-region)

(global-set-key (kbd "C-p") 'ibuffer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x") 'kill-region-or-bust)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "<f3>") 'isearch-repeat-forward)
(global-set-key (kbd "S-<f3>") 'isearch-repeat-backward)
(global-set-key (kbd "M-<f4>") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-<f4>") 'my-kill-this-buffer)
(global-set-key (kbd "C-w") 'my-kill-this-buffer)
(global-set-key (kbd "<kp-6>") 'split-window-right)
(global-set-key (kbd "<kp-2>") 'split-window-below)
(global-set-key (kbd "<kp-0>") 'delete-window)
(global-set-key (kbd "<kp-1>") 'delete-other-windows)
(global-set-key (kbd "<kp-3>") 'my-compilation-window-setup)
(global-set-key (kbd "<kp-add>") 'enlarge-window)
(global-set-key (kbd "<kp-subtract>") 'shrink-window)
(global-set-key (kbd "<kp-multiply>") 'enlarge-window-horizontally)
(global-set-key (kbd "<kp-divide>") 'shrink-window-horizontally)

;; Escape ALWAYS quits. Ctrl-g can bite me.
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-g") 'use-escape-to-cancel)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map (kbd "C-g") 'use-escape-to-cancel)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map (kbd "C-g") 'use-escape-to-cancel)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map (kbd "C-g") 'use-escape-to-cancel)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map (kbd "C-g") 'use-escape-to-cancel)
(global-set-key [escape] 'keyboard-quit)
(global-set-key (kbd "C-g") 'goto-line)

;; Now, Emacs. I win binding contests vs specific modes.
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") 'kill-ring-save-or-bust)
    (define-key map (kbd "C-s") 'save-buffer)
    (define-key map (kbd "C-h") 'replace-string)
    (define-key map (kbd "C-o") 'find-file)
    (define-key map (kbd "C-<tab>") 'other-window)
    (define-key map [(shift delete)] 'kill-line)
    map)
  "my-keys-minor-mode keymap.")
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override insistant major modes."
  :init-value t
  :lighter " Sane")
(my-keys-minor-mode 1)

;;---------------------------------------------------------------------------
;;
;; Oxpig/oxlang modes.
;;

;; Oxpig mode
(autoload 'oxpig-mode "oxpig-mode" "Oxpig mode." t)
(add-to-list 'auto-mode-alist '("\\.g\\'" . oxpig-mode))

;; Stupid indent mode
(require 'stupid-indent-mode)

;; Oxlang mode
(autoload 'oxlang-mode "oxlang-mode" "Oxlang mode." t)
(add-to-list 'auto-mode-alist '("\\.ox\\'" . oxlang-mode))

(defun oxnuke()
  "Unload and reapply oxlang mode."
  (interactive)
  (unload-feature 'oxlang-mode)
  (oxlang-mode))

(defun my-oxlang-mode-hook ()
  (setq stupid-indent-level 4)
  (stupid-indent-mode)
  )

(add-hook 'oxlang-mode-hook 'my-oxlang-mode-hook)

;;---------------------------------------------------------------------------
;;
;; Configuration from M-x customize-face et al.
;; Note: C-u M-x what-cursor-position = display face
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (multiple-cursors))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#f66" :foreground "black"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "OliveDrab3" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "OliveDrab3" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#8dd"))))
 '(font-lock-function-name-face ((t (:foreground "#ec7"))))
 '(font-lock-keyword-face ((t (:foreground "#81a2be"))))
 '(font-lock-string-face ((t (:foreground "#c77"))))
 '(font-lock-type-face ((t (:foreground "#4bc"))))
 '(font-lock-variable-name-face ((t (:foreground "#ec7"))))
 '(org-level-1 ((t (:inherit outline-1 :foreground "#49c" :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "#ccc" :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "#7b7"))))
 '(show-paren-match ((t (:background "OliveDrab3" :foreground "#000"))))
 '(whitespace-empty ((t (:background "#633" :foreground "#fc5"))))
 '(whitespace-trailing ((t (:background "#633" :foreground "#fc5")))))
