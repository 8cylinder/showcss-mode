;;; buffer-combine.el
;;; show-css.el --- Show the css of the html attribute the cursor is on
;;
;; Copyright (C) 2012 Sheldon McGrandle
;;
;; Author: Sheldon McGrandle <developer@rednemesis.com>
;; Version: 0.1
;; Created: 12th April 2013
;; Keywords: utility
;; URL: https://github.com/smmcg/showcss-mode
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; author of this program or to the Free Software Foundation, 675 Mass
;; Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; DESCRIPTION AND USAGE
;;
;; (let ((buf (generate-new-buffer "Some name for display")))
;;   (set-buffer buf)         ;create a new buffer and set focus to it
;;   (buffer-combine-mode)    ;turn on buffer combine mode
;;   (bc/start data :readonly nil :hidden nil))  ;send it the data
;;
;; Data structure bc/start requires:
;; '((filename-or-buffer (start end) (start end) ...)
;;   (another-filename-or-buffer (start end) ...)
;;   (...))
;;
;; INSTALLATION
;;
;; BUGS


;;; Code:

(require 'cl-lib)
(require 's)


(defgroup buffer-combine nil
 "Customize buffer-combine"
 :prefix "buffer-combine"
 :group 'Text)

(defface buffer-combine/region-face
 '((t (:background "grey50")))
 "Highlight the full selector"
 :group 'buffer-combine)


(defvar bc/buffers-data nil
  "data format:
'((filename overlay1 overlay2 overlay3)
  (anotherfile overlay1 overlay2))")
;(make-local-variable bc/buffers-data)

(defvar bc/this-buffer nil)
;(make-local-variable bc/this-buffer)


(defun* bc/start (data &key (readonly nil) (hidden nil))
  "Recieve and parse the data
two optional flags readonly and hidden"
  (bc/remove-source-overlays)
  (let ((buffers-data '()))
    ;;for each file and its fragment positions:
    (dolist (filelist data)
      (let ((buffer (bc/load-file (car filelist) hidden)))
        ;;for each fragment position:
        (setq buffers-data
              (cons (bc/mark-fragments-in-source buffer (cdr filelist))
                    buffers-data))))
    (setq bc/buffers-data buffers-data)
    (bc/build-display buffers-data)))


(defun bc/load-file (file hidden)
  "Load the files from disk and if hidden is t,
rename them with a space in front of the buffer title"
  (let ((return-buffer nil))
    ;; if file is actually a buffer, do nothing
    (if (bufferp file)
        ;; return: <file>
        (setq return-buffer file)
      ;; since file is a string, see if its
      ;; already loaded in a buffer
      (dolist (b (buffer-list))
        (if (string= (buffer-file-name b) (file-truename file))
            (setq return-buffer b))))
    ;; since file is not a buffer and its not
    ;; already loaded, load it now
    (if (not return-buffer)
        (setq return-buffer (find-file-noselect file)))

    (set-buffer return-buffer)
    (if hidden
        (rename-buffer (concat " " (s-trim-left (buffer-name))))
      ;; if file is *not* to be hidden but it is already,
      ;; unhide it by removing the space at the begining.
      (if (s-starts-with? " " (buffer-name))
          (rename-buffer (s-trim-left (buffer-name)))))

    ;;return: <buffer handle>
    return-buffer))


(defun bc/mark-fragments-in-source(buffer fragments)
  "Iterate over all the fragments in one buffer"
  (set-buffer buffer)
  ;(switch-to-buffer buffer)
  (remove-overlays)
  (let ((buffer-overlay-list (cons buffer '())))
    (dolist (fragment fragments)
      (setq buffer-overlay-list
            (cons
             (bc/mark-fragment-in-source
              buffer
              (nth 0 fragment)   ;start
              (nth 1 fragment))  ;end
             buffer-overlay-list)))
    (reverse buffer-overlay-list)))


(defun bc/mark-fragment-in-source (buffer start end)
  "Mark a fragments in a buffer with an overlay"
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face 'buffer-combine/region-face)
    (overlay-put ov 'before-string
                 "\nChanges made here will be overwritten by
edits made in the Show CSS buffer:\n")
    (overlay-put ov 'after-string
                 "-----------------------------------------\n")
    ;; return <ov>:
    ov))


(defun bc/build-display(buffers-data)
  "Build the display for each fragment"
  (set-buffer bc/this-buffer)
  (remove-overlays)
  (erase-buffer)
  (dolist (file-and-overlays buffers-data)
    (let ((buf (car file-and-overlays)))
      (insert (format "\n\n/* %s */\n" (file-name-nondirectory
                                        (buffer-file-name buf))))
      (dolist (source-ov (cdr file-and-overlays))
        (let* ((source-start (overlay-start source-ov))
               (source-end (overlay-end source-ov))
               (display-length (- source-end source-start)))
          (insert-buffer-substring-no-properties
           buf
           (overlay-start source-ov)
           (overlay-end source-ov))
          (let ((display-ov (make-overlay
                             (point) (- (point) display-length) nil nil nil)))
            (overlay-put display-ov 'before-string "\n")
            (overlay-put display-ov 'modification-hooks
                         '(bc/send-back-to-source))
            (overlay-put display-ov 'source-overlay source-ov)
            (overlay-put display-ov 'face 'buffer-combine/region-face))
          ;(message "%s %s" (char-after (overlay-end display-ov)) "overlay:")
          ;(insert "\n")
          ))))
  (goto-char (point-min)))


(defun bc/send-back-to-source(ov &optional flag &rest rv)
  "Any edits made in the display buffer are sent back to the
linked overlay in the source buffer"
  (if flag
      (progn (let*
        ((source-ov (overlay-get ov 'source-overlay))
         (source-start (overlay-start source-ov))
         (source-end (overlay-end source-ov))
         (display-start (overlay-start ov))
         (display-end (overlay-end ov))
         (content (buffer-substring-no-properties
                   display-start display-end)))
        (set-buffer (overlay-buffer source-ov))
        (goto-char source-start)
        (insert content)
        (delete-region (point) (overlay-end source-ov))))))


(defun bc/save-source-buffers()
  (interactive)
  "Save all the source buffers"
  (message "saving source buffers")
  (dolist (buffer-and-fragments bc/buffers-data)
    (let ((buffer (car buffer-and-fragments)))
      (set-buffer buffer)
      (save-buffer)
  )))


(defun bc/remove-source-overlays()
  "Remove all the overlays from the source buffers"
  (dolist (buffer-and-fragments bc/buffers-data)
    (let ((buffer (car buffer-and-fragments)))
      (set-buffer buffer)
      (dolist (ov (cdr buffer-and-fragments))
        (delete-overlay ov))
  )))


(defvar buffer-combine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (read-kbd-macro "C-x C-s") 'bc/save-source-buffers)
    map)
  "some documentation")


;; derive from css-mode, sass-mode?
(define-derived-mode buffer-combine-mode css-mode "Combine"
  "Display fragments from other buffers
\\{buffer-combine-mode-map}"

  (setq bc/this-buffer (current-buffer))
  (add-hook 'kill-buffer-hook 'bc/remove-source-overlays nil t))

;  (if buffer-combine-mode
;      (progn
;        (setq bc/this-buffer (current-buffer))
;        (add-hook 'kill-buffer-hook 'bc/remove-source-overlays nil t))
;    (bc/remove-source-overlays))
;)


(provide 'buffer-combine)

;;; buffer-combine-mode.el ends here
