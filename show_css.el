;;; COPYRIGHT NOTICE
;;
;; Copyright (C) 2012 Sheldon McGrandle.
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

;; Send bug reports to Sheldon McGrandle <developer@rednemesis.com>


;;; DESCRIPTION AND USAGE
;;

;;; INSTALLATION
;;

;;; BUGS
;;



(defgroup showcss nil
  "Customize showcss"
  :prefix "showcss/"
  :group 'Text)

(defface showcss/css-face
  '((t :background "green"))
  "Highlight the matched selector in the css file"
  :group 'showcss)

(defface showcss/html-face
  '((t :background "green"))
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
(defvar showcss/last-html-overlay (make-overlay 0 0)
  "this is the last overlay set in the html file")
(defvar showcss/css-buffer nil
  "The buffer that contains the css file")
(defvar showcss/html-buffer nil
  "The buffer that contains the html file")


(defun showcss/set-css-buffer()
  "showcss will look for css files in the following places:
1.  Look for values set in customize.
2.  Look for the <!-- showcss: ... --> in the html file
3.  Look at the css declarations in the html <head>

Showcss will only use local files.  So if you use css on a remote
server, you will need to use the showcss tag in you html file and
have it point to a local copy of that css.

Find the name of the css file using this regex:
<!-- showcss: \\(.*\\) -->
Eg:
<!-- showcss: /home/sm/projects/some project/site/css/main.css -->"

  (setq showcss/csslist nil)
  (setq showcss/css-buffer nil)

  (if showcss/use-html-tags
      ;; get the <link> css
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
                            (setq showcss/csslist (cons css-file showcss/csslist)))))))
            (goto-char tag-end)))))

  ;; get the <!-- showcss ... --> comment if any
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "<!-- showcss: \\(.*\\) -->" nil t)
      (if (file-exists-p (match-string 1))
          (setq showcss/csslist
                (cons (substring-no-properties (match-string 1)) showcss/csslist)))))

  ;; load the css files into buffers
  (mapc (lambda (css-file)
          (setq showcss/css-buffer
                (cons
                 (find-file-noselect css-file)
                 showcss/css-buffer)))
        showcss/csslist))


(defun showcss/what-am-i()
  "What is the cursor on?  Should return class,
id, or nil and the class name or id name"
  (showcss/remove-highlights)
  (setq saved-point (point))

  (re-search-backward "[ \t\n]" nil t)
  (re-search-forward " \\(\\(id\\|class\\)=\"\\(.*?\\)\"\\)" nil nil 1)
  (goto-char saved-point)
  ;; is the saved-point between (match-beginning 0) and (match-end 0)?
  (if (and (> saved-point (match-beginning 1))
		   (< saved-point (match-end 1)))
	  (progn
		(showcss/highlight-html-selector (match-beginning 3) (match-end 3))
		;; RETURN: (selector type, selector name)
		(list
		 (substring-no-properties (match-string 2))
		 (substring-no-properties (match-string 3))))

	;; RETURN: (nil, nil)
	(list nil nil)))


(defun showcss/scroll-to-selector (css-values)
  "Scroll the css file to show the selector"
  (setq selector-type (nth 0 css-values))
  (setq selector-name (nth 1 css-values))
  ;(setq template "\\(%s\\)[ \n{]")
  (setq full-selector nil)

  (cond ((string= selector-type "class")
		 (setq full-selector
			   (concat "." selector-name)))
		((string= selector-type "id")
		 (setq full-selector
			   (concat "#" selector-name)))
		(t
		 (error (format "Wrong type of selector: %s" selector-type)))
		)
  (setq full-re-selector (format "\\(%s\\)[ ,\n{]" full-selector))
  (setq html-buffer (current-buffer))
  (catch 'break
	(dolist (css-buffer showcss/css-buffer found)
	  ;;(switch-to-buffer-other-window css-buffer)
	  (set-buffer css-buffer)
      ;; save current point so that if search doesn't find
      ;; anything, we can return to last point so that the buffer
      ;; doesn't scroll to the top
	  (setq saved-point (point))
	  (goto-char (point-min))
	  (if (re-search-forward full-re-selector nil t)
		  (progn
			(showcss/highlight-css-selector (match-beginning 1) (match-end 1))
            (switch-to-buffer-other-window css-buffer)
            (goto-char (match-beginning 1))
            (switch-to-buffer-other-window html-buffer)
            (message "")
			(setq fount t)  ; why is this here?
			(throw 'break t))
		(goto-char saved-point)
		(setq found nil)
		(message "Not found: %s" full-selector)))))


(defun showcss/highlight-css-selector (start end)
  "Highlight the matched selector"
  (delete-overlay showcss/last-css-overlay)
  (setq ov (make-overlay start end))
  (overlay-put ov 'face 'showcss/css-face)
  (setq showcss/last-css-overlay ov))


(defun showcss/highlight-html-selector (start end)
  "Highlight the current selector in the html file"
  (delete-overlay showcss/last-html-overlay)
  (setq ov (make-overlay start end))
  (overlay-put ov 'face 'showcss/html-face)
  (setq showcss/last-html-overlay ov))


(defun showcss/remove-highlights()
  "remove the last highlight from the html buffer"
  (delete-overlay showcss/last-html-overlay)
  (delete-overlay showcss/last-css-overlay))


(defun showcss/main()
  (interactive)
  ""
  (setq css-values (showcss/what-am-i))
  ;; if is a selector:
  (if (or (string= (nth 0 css-values) "class")
		  (string= (nth 0 css-values) "id"))
	  (progn  ;; then
		(showcss/scroll-to-selector css-values))
	;; else:
	;;  remove overlays
	(showcss/remove-highlights)))


(defun showcss/keymove()
  ""
  (if showcss-mode
	  (showcss/main)))


(defvar showcss-map nil)
(when (null showcss-map)
  (setq showcss-map (make-sparse-keymap))
  (define-key showcss-map (kbd "C-c C-c") 'showcss/main)
  (define-key showcss-map (kbd "C-c C-p") 'showcss/pop))

(define-minor-mode showcss-mode
  "Display the css of the class or id the cursor is at"

  :init-value nil
  :lighter " Show"
  :keymap showcss-map

  (if showcss-mode
	  (progn
        ;; set the css buffer
        (setq showcss/html-buffer
              (current-buffer))
        (showcss/set-css-buffer)

		(defadvice next-line (after showcss/advise-main)
		  "Advice around cursor movement"
		  (showcss/keymove))
		(defadvice previous-line (after showcss/advise-main)
		  "Advice around cursor movement"
		  (showcss/keymove))
		(defadvice right-char (after showcss/advise-main)
		  "Advice around cursor movement"
		  (showcss/keymove))
		(defadvice left-char (after showcss/advise-main)
		  "Advice around cursor movement"
		  (showcss/keymove))
		(defadvice forward-word (after showcss/advise-main)
		  "Advice around cursor movement"
		  (showcss/keymove))
		(defadvice backward-word (after showcss/advise-main)
		  "Advice around cursor movement"
		  (showcss/keymove))
        (defadvice mouse-set-point (after showcss/advise-main)
		  "Advice around cursor movement"
		  (showcss/keymove))

		(ad-activate 'next-line)
		(ad-activate 'previous-line)
		(ad-activate 'right-char)
		(ad-activate 'left-char)
		(ad-activate 'forward-word)
		(ad-activate 'backward-word)
        (ad-activate 'mouse-set-point))

	(showcss/remove-highlights)
	(ad-deactivate 'next-line)
	(ad-deactivate 'previous-line)
	(ad-deactivate 'right-char)
	(ad-deactivate 'forward-word)
	(ad-deactivate 'backward-word)
	(ad-deactivate 'left-char)
    (ad-deactivate 'mouse-set-point)))
