;;; flymenu.el --- Transient interface for flymenu -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/flymenu
;; Version: 0.1.0
;; Keywords: lisp tools
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Transient interface for flymenu

;;; Code:

(require 'transient)

(defcustom flymenu-known-flymake-backends '((elisp-flymake-byte-compile
                                             (:if-mode . emacs-lisp-mode)
                                             (:transient . t))
                                            (elisp-flymake-checkdoc
                                             (:if-mode . emacs-lisp-mode)
                                             (:key . "d")
                                             (:transient . t)))
  "Alist of (FLYMAKE-BACKEND (TRANSIENT-KEYWORD . VALUE)).
The car FLYMAKE-BACKEND is a symbol to add or remove
to `flymake-diagnostic-functions'.
Optional cdr is an alist of transient suffix props, e.g. :if-mode,
:if-derived etc."
  :group 'flymake
  :type '(alist
          :key-type (function :tag "Flymake backend"
                     ignore)
          :value-type
          (alist :options ((:key (radio
                                  (const :tag "Auto" auto)
                                  (string :tag "Toggle character ")))
                           (:command (radio
                                      (const :tag "Auto" nil)
                                      (symbol :tag "Custom Suffix")))))))

(defun flymenu-builder-shared-start (s1 s2)
  "Return the longest prefix S1 and S2 have in common."
  (declare (pure t)
           (side-effect-free t))
  (let ((search-length (min (length s1)
                            (length s2)))
        (i 0))
    (while (and (< i search-length)
                (= (aref s1 i)
                   (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun flymenu-builder-capitalize-variants (word)
  "Return list of words of WORD, but it with upcased letter."
  (let ((cands)
        (parts (split-string word "" t)))
    (dotimes (i (length parts))
      (let ((val (string-join (remove nil (list
                                           (when (> i 0)
                                             (string-join (seq-take parts i) ""))
                                           (upcase (nth i parts))
                                           (string-join (seq-drop parts (1+ i))
                                                        "")))
                              "")))
        (push val
              cands)))
    (reverse cands)))

(defun flymenu-builder-safe-substring (len word)
  "Substring WORD from zero to LEN."
  (if (> (length word) len)
      (substring-no-properties word 0 len)
    word))

(defun flymenu-builder-get-all-key-strategies (word len)
  "Generate preffered shortcut from WORD with length LEN."
  (let* ((parts (append (split-string word "[^a-z]" t)
                        (list (replace-regexp-in-string "[^a-z]" "" word))))
         (parts-len (length parts))
         (finalize (lambda (short)
                     (while (> len (length short))
                       (setq short (concat short (number-to-string (random 10)))))
                     (flymenu-builder-safe-substring len short)))
         (vars
          (mapcar finalize (flymenu-builder-capitalize-variants
                            (flymenu-builder-safe-substring len
                                                            (replace-regexp-in-string
                                                             "[^a-z]"
                                                             ""
                                                             word))))))
    (seq-sort-by
     (lambda (it)
       (cond ((string-match-p "[0-9]" it)
              -2)
             ((member it vars)
              -1)
             (t (length (flymenu-builder-shared-start word it)))))
     #'>
     (seq-uniq (append
                vars
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat
                                      (apply-partially
                                       #'flymenu-builder-safe-substring n)
                                      parts)))
                 (number-sequence 1 (min len parts-len)))
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat
                                      (apply-partially
                                       #'flymenu-builder-safe-substring n)
                                      (reverse parts))))
                 (number-sequence 1 (min len parts-len))))))))

(defun flymenu-builder-generate-shortcuts (items &optional key-fn value-fn
                                                 used-keys)
  "Generate shortcuts from list of ITEMS.
If KEY-FN is nil, ITEMS should be list of strings or symbols.
If KEY-FN is a function, it will be called with every item of list, and should
return string that will be as basis for shortcut.
If VALUE-FN is nil, result is an alist of generated keys and corresponding
items.
If VALUE-FN is non nil, return a list of results of calling VALUE-FN with two
arguments - generated shortcut and item.
USED-KEYS is a list of keys that shouldn't be used."
  (let* ((value-fn (or value-fn (lambda (key value)
                                  (if (proper-list-p value)
                                      (append (list key) value)
                                    (cons key value)))))
         (total (length items))
         (random-variants (append
                           (mapcar #'char-to-string
                                   (number-sequence (string-to-char
                                                     "a")
                                                    (string-to-char
                                                     "z")))
                           (mapcar #'char-to-string
                                   (number-sequence (string-to-char
                                                     "A")
                                                    (string-to-char
                                                     "Z")))))
         (variants-len (length random-variants))
         (min-len
          (if used-keys
              (length (car (seq-sort-by #'length #'> used-keys)))
            (cond ((>= variants-len total)
                   1)
                  ((>= variants-len (/ total 2))
                   2)
                  (t 3)))))
    (let ((shortcuts used-keys)
          (used-words '())
          (result))
      (dotimes (i (length items))
        (let* ((def (nth i items))
               (word (if key-fn
                         (funcall key-fn def)
                       (if (symbolp def)
                           (symbol-name def)
                         def))))
          (when (not (member word used-words))
            (push word used-words)
            (let ((short
                   (downcase
                    (substring-no-properties word 0
                                             (min min-len
                                                  (length word))))))
              (setq short (replace-regexp-in-string "[^a-z]" "" short))
              (setq short
                    (seq-find
                     (lambda (it)
                       (not
                        (seq-find (apply-partially
                                   #'string-prefix-p it)
                                  shortcuts)))
                     (append
                      (flymenu-builder-get-all-key-strategies word
                                                              min-len)
                      (when (= min-len 1)
                        random-variants))))
              (while (and
                      (or (< (length short) min-len)
                          (member short shortcuts)))
                (setq short (concat short (number-to-string (random 10)))))
              (push short shortcuts)
              (push
               (cond ((functionp value-fn)
                      (funcall value-fn short def))
                     (t (cons short def)))
               result)))))
      (reverse result))))

(defun flymenu-toggle-backend (backend)
  "Toggle flymake BACKEND."
  (flymake-mode -1)
  (if (memq backend flymake-diagnostic-functions)
      (remove-hook 'flymake-diagnostic-functions backend t)
    (add-hook 'flymake-diagnostic-functions
              backend nil t))
  (flymake-mode 1))

(defun flymenu-get-descriptions-width ()
  "Return align width for flymake backends."
  (+ 5 (or
        (apply #'max
               (remove nil
                       (mapcar
                        (lambda (it)
                          (length
                           (or
                            (let ((descr
                                   (cdr
                                    (assq :description
                                          (cdr
                                           (assq
                                            it
                                            flymenu-known-flymake-backends))))))
                              (when (stringp descr)
                                descr))
                            (symbol-name it))))
                        (remove t
                                (append
                                 flymake-diagnostic-functions
                                 (mapcar #'car
                                         flymenu-known-flymake-backends))))))
        10)))

(defun flymenu-get-suffixes (&optional used-keys)
  "Generate a list of flymake backend shortcuts and properties.

Optional argument USED-KEYS is a list of keys that shouldn't be used."
  (let* ((key-defined-backends (seq-filter
                                (lambda (it)
                                  (stringp (cdr (assq :key (cdr it)))))
                                flymenu-known-flymake-backends))
         (diags
          (seq-uniq
           (seq-difference
            (append
             (remove t flymake-diagnostic-functions)
             (mapcar #'car
                     (seq-remove (pcase-lambda (`(,_k . ,v))
                                   (stringp (cdr (assq :key v))))
                                 flymenu-known-flymake-backends)))
            (mapcar #'car key-defined-backends))))
         (all-used-keys
          (delete-dups (delq nil (append
                                  (mapcar
                                   (pcase-lambda
                                     (`(,_k .
                                        ,v))
                                     (when (stringp
                                            (cdr
                                             (assq
                                              :key
                                              v)))
                                       (cdr (assq :key v))))
                                   flymenu-known-flymake-backends)
                                  used-keys))))
         (shortcuts (flymenu-builder-generate-shortcuts
                     diags
                     (lambda (it)
                       (replace-regexp-in-string
                        "flymake\\|check" ""
                        (symbol-name
                         it)))
                     (lambda (key value)
                       (let
                           ((props
                             (seq-remove (pcase-lambda (`(,k . ,_v))
                                           (eq k :key))
                                         (cdr
                                          (assq value
                                                flymenu-known-flymake-backends)))))
                         (cons value
                               (append
                                (list (cons :key key))
                                props))))
                     all-used-keys))
         (key-shortcuts (append key-defined-backends shortcuts))
         (align-width (flymenu-get-descriptions-width)))
    (mapcar (lambda (cell)
              (let* ((backend (car cell))
                     (props (cdr cell))
                     (name (symbol-name backend))
                     (key (cdr (assq :key props)))
                     (cmd (cdr (assq :command props)))
                     (sym (or cmd
                              (make-symbol (concat
                                            "flymenu-toggle-"
                                            name))))
                     (descr (cdr (assq
                                  :description
                                  props)))
                     (description
                      (cond ((stringp descr)
                             descr)
                            ((functionp descr)
                             descr)
                            (t name))))
                (unless cmd
                  (fset sym `(lambda ()
                               (interactive)
                               (ignore-errors
                                 (flymenu-toggle-backend
                                  ',backend)))))
                (append (list key sym
                              :description
                              (if (functionp description)
                                  description
                                `(lambda ()
                                   (concat
                                    (propertize
                                     ,(truncate-string-to-width
                                       (substring-no-properties description)
                                       align-width
                                       0 ?\ )
                                     'face
                                     (if
                                         (memq ',backend
                                          flymake-diagnostic-functions)
                                         'success nil))
                                    (if (memq ',backend
                                         flymake-diagnostic-functions)
                                        "[X]" "[ ]"))))
                              :transient t)
                        (mapcar (pcase-lambda (`(,k . ,v))
                                  (list k v))
                                (seq-remove
                                 (lambda (it)
                                   (memq (car it)
                                         '(:command :key
                                           :description)))
                                 props)))))
            key-shortcuts)))

;;;###autoload (autoload 'flymenu-backends-menu "flymenu.el" nil t)
(transient-define-prefix flymenu-backends-menu ()
  "Menu for toggling flymake backends.

Suffixes are generated dynamically from currently active checkers and
`flymenu-known-flymake-backends'."
  ["Flymake"
   :setup-children
   (lambda (_args)
     (transient-parse-suffixes
      transient--prefix
      (apply #'vector
             (flymenu-get-suffixes))))])

;;;###autoload
(defun flymenu-toggle-flymake-mode ()
  "Toggle Flymake mode and set up its transient menu."
  (interactive)
  (call-interactively #'flymake-mode)
  (transient-setup 'flymenu-flymake))

(defun flymenu--extract-keys-from-layout (layout)
  "Extract keys from a transient layout.

Argument LAYOUT is a list structure representing the key layout from which keys
are to be extracted."
  (let ((res)
        (stack (seq-remove #'not layout)))
    (while stack
      (let ((curr (pop stack))
            (key))
        (pcase curr
          ((pred (vectorp))
           (push (append curr nil) stack))
          ((pred (not (listp)))
           nil)
          ((guard (setq key (plist-get curr :key)))
           (push key res))
          ((guard curr)
           (setq stack (append stack curr))))))
    res))

;;;###autoload (autoload 'flymenu-flymake "flymenu.el" nil t)
(transient-define-prefix flymenu-flymake ()
  "Menu with flymake commands and enabling/disabling flymake backends.
Backends are generated dynamically from currently active checkers and
`flymenu-known-flymake-backends'."
  ["Flymake"
   ("M" flymenu-toggle-flymake-mode
    :description
    (lambda ()
      (let ((on (bound-and-true-p flymake-mode)))
        (propertize
         (concat (truncate-string-to-width
                  "Toggle Flymake Mode"
                  (flymenu-get-descriptions-width)
                  0 ?\ )
                 (if on "[X]" "[ ]"))
         'face
         (if on 'success nil))))
    :transient nil)
   ("C" "Check now" flymake-start
    :inapt-if-not (lambda ()
                    (and (boundp 'flymake-mode)
                         (symbol-value 'flymake-mode)))
    :transient t)
   ("B" "Buffer problems" flymake-show-buffer-diagnostics
    :inapt-if-not (lambda ()
                    (and (boundp 'flymake-mode)
                         (symbol-value 'flymake-mode))))
   ("R" "Project diagnostic" flymake-show-project-diagnostics)
   ("L" "Show logs" flymake-switch-to-log-buffer)
   ""
   ("M-n" "Next error" flymake-goto-next-error :transient t)
   ("M-p" "Previous error" flymake-goto-prev-error :transient t)]
  ["Toggle checkers"
   :setup-children
   (lambda (&rest _)
     (let ((used-keys
            (condition-case nil
                (seq-remove
                 (apply-partially #'string-match-p
                                  "\\`\\(\\(C-[chx] \\|M-[gso] \\)\\([CM]-\\)?\\|.+-\\)")
                 (reverse
                  (flymenu--extract-keys-from-layout
                   (get
                    'flymenu-flymake
                    'transient--layout))))
              (error '("M" "C" "B" "R" "L")))))
       (transient-parse-suffixes
        transient--prefix
        (apply #'vector
               (flymenu-get-suffixes
                used-keys)))))])

(provide 'flymenu)
;;; flymenu.el ends here
