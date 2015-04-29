;;; shell-select.el --- Fast shell selection

;; Copyright (C) 2015 Tim Perkins

;; Author: Tim Perkins
;; Maintainer: Tim Perkins
;; Created: April 1st, 2015
;; Version: 0.1
;; Keywords: shell

;; This program is free software. You can use, modify, and redistribute it
;; under the terms of the MIT License.

;;; Commentary:

;; This extension provides a quick and easy way to use multiple shells.

;; Launching shells in Emacs is harder then it should be: you have to run the
;; shell command, if you want to name the shell you have to provide a prefix
;; argument, and then when you name the shell you probably want to type
;; asterisks and what-not so it looks good.

;; This extension helps alleviate those annoyances by doing some of the work
;; for you. When launching a shell, you will always be prompted for a name. If
;; the name looks kind of plain, it will be fixed up to look like a shell name.
;; (So something like "debug" will get turned into "*debug-shell*". Cool right?
;; Then when you want to switch shells, you will get choice of just the shell
;; buffers, with completion. I recommend using Ido completion to make it even
;; better.

;; There is only one function in this extension: `shell-select-switch-shell'.
;; You should probably bind it to a really awesome key, like <f1> maybe.

;;; Code:

;; TODO Add support for other shells, like eshell and term...

(defvar shell-select-completing-read-function nil
  "Completing-read-function for shell selection.")

(defvar ssel--shell-history nil
  "The history for `completing-read' to get shells.")

(defun ssel--shell-buffer-p (buffer-or-name)
  "Test if BUFFER-OR-NAME is a buffer running a shell."
  (eq (buffer-local-value 'major-mode (get-buffer buffer-or-name))
      'shell-mode))

(defun ssel--get-shell-buffers (&optional frame)
  "Gets a list of all shells buffers.

If a FRAME is supplied, only get the shell buffer for that frame.
The shell buffers will be in the same order as the fundamental
buffer list."
  (cl-remove-if-not #'ssel--shell-buffer-p (buffer-list frame)))

(defun ssel--sort-shell-buffers (shell-buffers)
  "Sort the list of SHELL-BUFFERS for selection.

Use the default ordering, which is the same as the fundamental
buffer list, but if the current buffer is a shell, put that one
last."
  (let ((current-buffer (current-buffer)))
    (if (equal (car shell-buffers) current-buffer)
        (append (delete current-buffer shell-buffers)
                (list current-buffer))
      shell-buffers)))

(defun ssel--format-shell-buffer-name (buffer-name)
  "Do various magic on BUFFER-NAME to make it look good."
  (when (or (not buffer-name) (string= buffer-name ""))
    (setq buffer-name "*shell*"))
  (unless (string-match-p "\\`\\*.*?\\*\\'" buffer-name)
    (unless (string-match-p "shell" buffer-name)
      (setq buffer-name (format "%s-shell" buffer-name)))
    (setq buffer-name (format "*%s*" buffer-name)))
  buffer-name)

(defun ssel--make-shell (buffer-name)
  "Make a shell with the name BUFFER-NAME."
  (shell buffer-name))

;;;###autoload
(defun shell-select-switch-shell (shell-buffer-or-name)
  "Switch to a new shell.

SHELL-BUFFER-OR-NAME can be a buffer, or the name of one. If the
shell doesn't exist, start one. If the name of the new shell
looks kind of plain, the name will be prettified. Something like
\"name\" will become \"*name-shell*\" by default."
  (interactive
   (list
    (let ((completing-read-function (or shell-select-completing-read-function
                                        completing-read-function))
          (shell-buffer-names (mapcar #'buffer-name
                                      (ssel--sort-shell-buffers
                                       (ssel--get-shell-buffers)))))
      (completing-read "Shell: " shell-buffer-names nil nil nil
                       'ssel--shell-history (car shell-buffer-names)))))
  (let ((shell-buffer (get-buffer shell-buffer-or-name)))
    ;; Make sure we really got a shell
    (unless (and shell-buffer (ssel--shell-buffer-p shell-buffer))
      (let ((shell-buffer-name (ssel--format-shell-buffer-name shell-buffer-or-name)))
        ;; Try again with the corrected name
        (setq shell-buffer (get-buffer shell-buffer-name))
        (unless (and shell-buffer (ssel--shell-buffer-p shell-buffer))
          ;; Didn't find anything, make a new shell
          (setq shell-buffer (ssel--make-shell shell-buffer-name)))))
    (switch-to-buffer shell-buffer)))

(provide 'shell-select)

;;; shell-select.el ends here
