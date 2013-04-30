;;; show-css.el --- Show the css of the html attribute the cursor is on
;;
;; Copyright (C) 2012 Sheldon McGrandle
;;
;; Author: Sheldon McGrandle <developer@rednemesis.com>
;; Version: 1.01
;; Created: 1st February 2013
;; Keywords: hypermedia
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
;; BUGS
;;
;; Please report any bugs found to the github repository
;; https://github.com/smmcg/showcss-mode
;;
;; Also, it you have ideas, suggestions, or advice; please
;; let me know.  Use the github issues tool or send me
;; an email.
;;

;;; Code:

(require 'buffer-combine)
(require 'dom)

(defgroup showcss nil
  "Customize showcss"
  :prefix "showcss/"
  :group 'Text)


(defcustom showcss/update-delay 1
  "Number of seconds of idle time from last keypress
before updating selectors display"
  :group 'showcss
  :type 'number)

(defcustom showcss/use-html-tags t
  "Use the <link ...> tag in addition to the <!-- --> comments.
Turn off if you want to only comments to explicitly set the css
to view"
  :group 'showcss
  :type 'boolean)


(defvar showcss/css-buffer nil
  "The buffer that contains the css file")
(make-local-variable 'showcss/css-buffer)

(defvar showcss/html-buffer nil
  "The buffer that contains the html file")
(make-local-variable 'showcss/html-buffer)

(defvar showcss/parents nil
  "The list of parents for the current tag")
(make-local-variable 'showcss/parents)

(defvar showcss/timer nil)
(make-local-variable 'showcss/timer)

(defvar showcss/display-buffer nil
  "The buffer to display the matches in")
(make-variable-buffer-local 'showcss/display-buffer)


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
              (if (re-search-forward
                   "\\(type=\"text/css\"\\|rel=\"stylesheet\"\\)" tag-end t)
                  (progn
                    (goto-char tag-start)
                    (if (re-search-forward "href=\"\\([^:]*?\\)\"" tag-end t)
                        (let ((css-file
                               (file-truename (substring-no-properties
                                               (match-string 1)))))
                          (if (file-exists-p css-file)
                              (setq csslist (cons css-file csslist)))))))
              (goto-char tag-end)))))

    ;; get the <!-- showcss ... --> comment if any
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<!-- showcss: \\(.*?\\) -->" nil t)
        (if (file-exists-p (match-string 1))
            (setq csslist
                  (cons (substring-no-properties
                         (match-string 1)) csslist)))))

    ;; load the css files into buffers
    (mapc (lambda (css-file)
            (setq showcss/css-buffer
                  (cons
                   (find-file-noselect css-file)
                   showcss/css-buffer)))
          csslist)))


(defun showcss/what-elements()
  "Get a list of elements from the current tag.
It returns the tag name, id and class lists.
For example:
\(\"div\" (\"id\" . \"someid\") (class \"dog\" \"cat\"))"
  (setq showcss/html-buffer (current-buffer))
  (save-excursion
    (re-search-backward "\\(>\\|</\\|<!\\|<\\)" nil t)
    (cond
     ;; > end of tag - not in a tag
     ((string= ">" (match-string 0))
      nil)
     ;; </ in a closing tag
     ((string= "</" (match-string 0))
      nil)
     ;; <! in a comment
     ((string= "<!" (match-string 0))
      nil)
     ;; < in an opening tag
     ((string= "<" (match-string 0))
      (let ((tag-start (match-end 1))
            (tag-end (search-forward ">"))
            (tag-name nil)
            (tag-id nil)
            (tag-class nil))
        ;; tag name
        (goto-char tag-start)
        (re-search-forward "[^ >]*" tag-end t nil)
        (setq tag-name (match-string-no-properties 0))
        ;; id
        (goto-char tag-start)
        (if (re-search-forward "id ?= ?\"\\(.*?\\)\"" tag-end t nil)
            (setq tag-id (match-string-no-properties 1)))
        ;; class
        (goto-char tag-start)
        (if (re-search-forward "class ?= ?\"\\(.*?\\)\"" tag-end t nil)
            (progn
              (setq tag-class (match-string-no-properties 1))
              (setq tag-class (s-split " " tag-class t))))

        ;; return tag info
        (list tag-name (cons 'id tag-id) (cons 'class tag-class)))))))


(defun showcss/find-selectors (elements)
  "For each source buffer, get a list
of positions of matched selectors"
  (let ((tag-name (car elements))
        (tag-id (cdr (nth 1 elements)))
        (tag-css (cdr (nth 2 elements)))
        (data nil)
        (html-buffer (current-buffer)))
    (dolist (source-buffer showcss/css-buffer)
      (let ((buffer-and-fragments ()))
        ;;(if tag-name
        ;;    (setq buffer-and-fragments
        ;;          (cons (showcss/get-points source-buffer 'tag tag-name)
        ;;                buffer-and-fragments)))
        (if tag-id
            (setq buffer-and-fragments
                  (cons (showcss/get-points source-buffer 'id tag-id)
                        buffer-and-fragments)))
        (if tag-css
            (setq buffer-and-fragments
                  (cons (showcss/get-points source-buffer 'class tag-css)
                        buffer-and-fragments)))
        (if (car buffer-and-fragments)
            (progn
              (setq buffer-and-fragments
                    (cons source-buffer
                          (car buffer-and-fragments)))
              (setq data (cons buffer-and-fragments data)))
          (setq buffer-and-fragments nil))
        ))
    (message "%s" data)
    (showcss/display-info data html-buffer)
))


(defun showcss/get-points(source-buffer type value)
  ""
  (let ((search-string (showcss/build-selector type value))
        (locations ()))
    (set-buffer source-buffer)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward search-string nil t)
        (setq locations (cons
          (list (match-beginning 0) (match-end 0)) locations))))
    locations))


(defun showcss/build-selector (type value)
  "Convert a class to \".class\" or an id to \"#id\"
and create a regex to be used for searching in the css files.
eg: \"\\\\(\\\\.some_class\\\\)[ ,\\n{]\""
  (let ((full-selector nil))
    (cond ((eq type 'class)
           (setq full-selector (concat "\\(\\." (s-join "\\|\\." value) "\\)")))
          ((eq type 'id)
           (setq full-selector (concat "#" value)))
          ((eq type 'tag)
           (setq full-selector value))
          (t
           (error (format "Wrong type of selector: %s" type))))
    (if full-selector
        (progn
          (message ".*?%s.*?[\0-\377[:nonascii:]]*?}[^*]" full-selector)
          (format ".*?%s.*?[\0-\377[:nonascii:]]*?}[^*]" full-selector)
          )
      nil)))


(defun showcss/display-info(data html-buffer)
  "Create a display buffer and send the data to it."
  (let ((display-buffer (get-buffer-create "Show CSS")))
    (set-buffer display-buffer)
      (switch-to-buffer-other-window display-buffer)
      (buffer-combine-mode)             ;should this be call each time?
      (bc/start data)
      (switch-to-buffer-other-window html-buffer))
)


(defun showcss/main()
  (interactive)
  (let ((elements (showcss/what-elements)))
    (message "%s" elements)
    ;; if is a selector:
    (if elements
        (progn
          (showcss/find-selectors elements))
      ;; remove overlays
      (set-buffer (get-buffer-create "Show CSS"))
      (buffer-combine-mode)
      (bc/start nil))
    ))

(defun showcss/timerfunc()
  ""
  (if (and (string= (buffer-name) (buffer-name showcss/html-buffer))
           (memq last-command
                 '(next-line
                   previous-line
                   right-char
                   left-char
                   forward-word
                   backward-word
                   forward-sexp
                  backward-sexp)))
      (showcss/main)))


;;;###autoload
(define-minor-mode showcss-mode
  "Display the css of the class or id the cursor is at.
Visit https://github.com/smmcg/showcss-mode to view the
git repository"

  :init-value nil
  :lighter " ShowCSS"
  :keymap '(([C-c C-u] . showcss/parse-html))

  (if showcss-mode
      (progn
        (showcss/set-css-buffer)
        (setq showcss/html-buffer (current-buffer))
        (setq showcss/timer
              (run-with-idle-timer
               showcss/update-delay t 'showcss/timerfunc))
        ;(add-hook 'after-save-hook 'showcss/parse-html nil t)
        )

    ;; else
    (cancel-timer showcss/timer)
    ))


(provide 'show-css)

;;; show-css.el ends here
