;;; show-css.el --- Show the css of the html attribute the cursor is on
;;
;; Copyright (C) 2012 Sheldon McGrandle
;;
;; Author: Sheldon McGrandle <developer@rednemesis.com>
;; Version: 1.0
;; Created: 01 Feb 1013
;; Keywords: languages, css
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
;; author of this program <mboyer@ireq-robot.hydro.qc.ca> or to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; DESCRIPTION AND USAGE
;;
;; Show CSS is a minor mode for emacs.
;;
;; With showcss-mode turned on, as you navigate around an HTML file the
;; matching css for that element will be displayed in another buffer.
;;
;; In the current html buffer, if you move the cursor over a class=".*?"
;; or id=".*?" a buffer will open with the external css file loaded and
;; scrolled to the matching selector.
;;
;; Show Css will look at the <link> tags and a custom comment tag to get
;; the location of external css files.
;;
;; Show Css looks for a comment with this regex:
;; <!-- showcss: \\(.*?\\) -->
;;
;; For example:
;; <!-- showcss: /home/user/projects/facebook/site/css/main.css -->
;; or
;; <!-- showcss: ./sass_files/main.sass-->
;;
;; The comment is useful if you want to use sass files directly instead
;; of compiling them.  Also showcss-mode will only use local files.  So
;; if you use css on a remote server, you will need to use the showcss
;; tag in you html file and have it point to a local copy of that css.
;;
;; INSTALLATION
;;
;; Put this in your init.el or .emacs file:
;;
;;   (autoload 'showcss-mode "show-css"
;;      "Display the css of the class or id the cursor is at" t)
;;
;; Personally, I find this mode to distracting to use all the time, so I
;; use this function to quickly toggle the mode on and off.
;;
;;   (defun sm/toggle-showcss()
;;     "Toggle showcss-mode"
;;     (interactive)
;;     (if (derived-mode-p
;;          'html-mode
;;          'nxml-mode
;;          'nxhtml-mode
;;          'web-mode
;;          'handlebars-mode)
;;         (showcss-mode 'toggle)
;;       (message "Not in an html mode")))
;;   (global-set-key (kbd "C-c C-k") 'sm/toggle-showcss)
;;

;;; Code:

(defgroup showcss nil
  "Customize showcss"
  :prefix "showcss/"
  :group 'Text)

(defface showcss/css-face
  '((t (:foreground "white" :background "darkgreen")))
  "Highlight the matched selector in the css file"
  :group 'showcss)

(defface showcss/html-matched-face
  '((t (:foreground "white" :background "darkgreen")))
  "Highlight the selector the cursor is in"
  :group 'showcss)

(defface showcss/html-unmatched-face
  '((t (:foreground "white" :background "darkred")))
  "Highlight the selector the cursor is in"
  :group 'showcss)

(defcustom showcss/use-html-tags
  t
  "Use the <link ...> tag in addition to the <!-- --> comments.
Turn off if you want to only comments to explicitly set the css
to view"
  :group 'showcss
  :type 'boolean)


(defvar showcss/last-css-overlay (make-overlay 0 0)
  "this is the last overlay set in the css file")
(make-variable-buffer-local 'showcss/last-css-overlay)

(defvar showcss/last-html-overlay (make-overlay 0 0)
  "this is the last overlay set in the html file")
(make-variable-buffer-local 'showcss/last-html-overlay)

(defvar showcss/css-buffer nil
  "The buffer that contains the css file")
(make-variable-buffer-local 'showcss/css-buffer)

(defvar showcss/html-buffer nil
  "The buffer that contains the html file")
(make-variable-buffer-local 'showcss/html-buffer)


(defun showcss/set-css-buffer()
  "showcss will look for css files in the following places:
1.  Look for the <!-- showcss: ... --> in the html file
2.  Look at the css declarations in the html <head>

Showcss will only use local files.  So if you use css on a remote
server, you will need to use the showcss tag in you html file and
have it point to a local copy of that css.

Find the name of the css file using this regex:
<!-- showcss: \\(.*?\\) -->
Eg:
<!-- showcss: /home/sm/projects/some project/site/css/main.css -->"

  (setq showcss/css-buffer nil)

  (let ((csslist nil))

    ;; get the <link> css
    (if showcss/use-html-tags
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "<link\\(.\\|\n\\)*?>" nil t)
            (let ((tag-start (match-beginning 0))
                  (tag-end (match-end 0)))
              (goto-char tag-start)
              (if (re-search-forward "\\(type=\"text/css\"\\|rel=\"stylesheet\"\\)" tag-end t)
                  (progn
                    (goto-char tag-start)
                    (if (re-search-forward "href=\"\\([^:]*?\\)\"" tag-end t)
                        (let ((css-file
                               (file-truename (substring-no-properties (match-string 1)))))
                          (if (file-exists-p css-file)
                              (setq csslist (cons css-file csslist)))))))
              (goto-char tag-end)))))

    ;; get the <!-- showcss ... --> comment if any
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<!-- showcss: \\(.*?\\) -->" nil t)
        (if (file-exists-p (match-string 1))
            (setq csslist
                  (cons (substring-no-properties (match-string 1)) csslist)))))

    ;; load the css files into buffers
    (mapc (lambda (css-file)
            (setq showcss/css-buffer
                  (cons
                   (find-file-noselect css-file)
                   showcss/css-buffer)))
          csslist)))


(defun showcss/what-am-i()
  "What is the cursor on?  Should return class,
id, or nil and the class name or id name"
  (let ((saved-point (point)))
    (save-excursion
    (re-search-backward "[ \t\n]" nil t)
    (if (re-search-forward " \\(\\(id\\|class\\)=\"\\(.*?\\)\"\\)" nil t 1)
        (progn
          (goto-char saved-point)
          ;; is the saved-point between (match-beginning 0) and (match-end 0)?
          (if (and (> saved-point (match-beginning 1))
                   (< saved-point (match-end 1)))
              (progn
                ;; RETURN: (selector type, selector name)
                (list
                 (substring-no-properties (match-string 2))
                 (substring-no-properties (match-string 3))
                 ;; pass on the position so the attribute can
                 ;; be set with the right face later:
                 (match-beginning 3)
                 (match-end 3)))

            ;; RETURN: (nil, nil)
            (list nil nil)))
      (list nil nil)))))


(defun showcss/scroll-to-selector (css-values)
  "Scroll the css file to show the selector"
  (let ((selector-type (nth 0 css-values))
        (selector-name (nth 1 css-values))
        (full-selector nil))

    (cond ((string= selector-type "class")
           (setq full-selector
                 (concat "." selector-name)))
          ((string= selector-type "id")
           (setq full-selector
                 (concat "#" selector-name)))
          (t
           (error (format "Wrong type of selector: %s" selector-type)))
          )
    (let ((full-re-selector (format "\\(%s\\)[ ,\n{]" full-selector))
          (html-buffer (current-buffer))
          (attribute-start (nth 2 css-values))
          (attribute-end (nth 3 css-values))
          (found nil))
      (catch 'break
        (dolist (css-buffer showcss/css-buffer)
          (set-buffer css-buffer)
          (delete-overlay showcss/last-css-overlay)
          ;; save current point so that if search doesn't find
          ;; anything, we can return to last point so that the buffer
          ;; doesn't scroll to the top
          (let ((saved-point (point)))
            (goto-char (point-min))
            (if (re-search-forward full-re-selector nil t)
                (progn
                  (showcss/highlight-css-selector (match-beginning 1) (match-end 1))
                  (switch-to-buffer-other-window css-buffer)
                  (goto-char (match-beginning 1))
                  (switch-to-buffer-other-window html-buffer)
                  (setq found t)
                  (message "")
                  (throw 'break t))
              (goto-char saved-point)
              (message "Not found: %s" full-selector)))))
      (set-buffer html-buffer)
      (if found
          (showcss/highlight-html-selector
           attribute-start attribute-end 'showcss/html-matched-face)
        (showcss/highlight-html-selector
         attribute-start attribute-end 'showcss/html-unmatched-face))
      )))


(defun showcss/highlight-css-selector (start end)
  "Highlight the matched selector"
  (delete-overlay showcss/last-css-overlay)
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face 'showcss/css-face)
    (setq showcss/last-css-overlay ov)))


(defun showcss/highlight-html-selector (start end html-face)
  "Highlight the current selector in the html file"
  (delete-overlay showcss/last-html-overlay)
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face html-face)
    (setq showcss/last-html-overlay ov)))


(defun showcss/remove-highlights()
  "remove all highlights from all buffers"
  (delete-overlay showcss/last-html-overlay)
  (dolist (css-buffer showcss/css-buffer)
    (set-buffer css-buffer)
    (delete-overlay showcss/last-css-overlay)))



(defun showcss/main()
  (let ((css-values (showcss/what-am-i)))
    ;; if is a selector:
    (if (or (string= (nth 0 css-values) "class")
            (string= (nth 0 css-values) "id"))
        (progn
          (showcss/scroll-to-selector css-values))
      ;; remove overlays
      (showcss/remove-highlights))))


(defun showcss/keymove()
  (if (memq this-command
            '(next-line
              previous-line
              right-char
              left-char
              forward-word
              backward-word
              forward-sexp
              backward-sexp))
      (showcss/main)))


(define-minor-mode showcss-mode
  "Display the css of the class or id the cursor is at"

  :init-value nil
  :lighter " Show"

    (if showcss-mode
        (add-hook 'post-command-hook 'showcss/keymove nil t)
    ;; else
    (remove-hook 'post-command-hook 'showcss/keymove t)))

(provide 'show-css)
;;; show-css.el ends here
