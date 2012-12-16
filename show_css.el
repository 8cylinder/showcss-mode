;; multiple classes? class="dog cat cow"
;;

(defgroup showcss nil
  "Customize showcss"
  :prefix "showcss/"
  :group 'Text)

(defface showcss/css-face
  '((t :background "grey"))
  "Highlight the matched selector in the css file"
  :group 'showcss)

(defface showcss/html-face
  '((t :background "grey"))
  "Highlight the selector the cursor is in"
  :group 'showcss)

(defcustom showcss/projects
  '(("html" . "css"))
  "A list of HTML files and their assosiated CSS files.
An html file can have more than one assosiated CSS file"
  ;:default nil
  :group 'showcss
  :type '(repeat
		  (cons (file :tag "HTML file")
				(file :tag "CSS file"))))

(defcustom showcss/default-css-dir
  "./css"
  "Subdir in project that contains the css files"
  :group 'showcss
  :type 'file)


; add customize to asocciate html files with a css file
; so the string doesn't need to be in the html file.

; ----------------------------------------------------

(defvar showcss/last-css-overlay (make-overlay 0 0)
  "this is the last overlay set in the css file")
(defvar showcss/last-html-overlay (make-overlay 0 0)
  "this is the last overlay set in the html file")
(defvar showcss/css-buffer nil
  "The buffer that contains the css file")
(defvar showcss/html-buffer nil
  "The buffer that contains the html file")

; ----------------------------------------------------


; 1) look for a set var in customize: html<-->css
; 2) look for "showcss" string in html file
; 3) use css tag in html file

(defun showcss/set-css-buffer()
  "showcss will look for css files in the following places:
1.  Look for values set in customize.
2.  Look for the <!-- showcss: ... --> in the html file
3.  Look at the css declarations in the html head

Showcss will only use local files.  So if you use css on a remote
server, you will need to use the showcss tag in you html file and
have it point to a local copy of that css.

Find the name of the css file using this regex:
<!-- showcss: \\(.*\\) -->
Eg:
<!-- showcss: /home/sm/projects/some project/site/css/main.css -->"
  (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "<!-- showcss: \\(.*\\) -->" nil t)
		()
	  (error "\"<!-- showcss: ... -->\" does not exist in this file")))

  (setq showcss/css-buffer (find-file-noselect (match-string 1))))


(defun showcss/what-am-i()
  "What is the cursor on?  Should return class,
id, or nil and the class name or id name"
  (showcss/remove-highlights)
  (setq saved-point (point))

  (re-search-backward "[ \t\n]" nil t)
  ;(re-search-forward " \\(id\\|class\\)=\"\\(.*?\\)\"" nil nil 1)
  (re-search-forward " \\(\\(id\\|class\\)=\"\\(.*?\\)\"\\)" nil nil 1)
  (goto-char saved-point)
  ; is the saved-point between (match-beginning 0) and (match-end 0)?
  (if (and (> saved-point (match-beginning 1))
		   (< saved-point (match-end 1)))
	  (progn
		(showcss/highlight-html-selector (match-beginning 1) (match-end 1))
		; RETURN: (selector type, selector name)
		(list
		 (substring-no-properties (match-string 2))
		 (substring-no-properties (match-string 3))))

	; RETURN: (nil, nil)
	(list nil nil)
))


(defun showcss/display-css()
  "Display the css buffer"
  ; TODO: check if css-file exists
  (display-buffer showcss/css-buffer))


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
  (setq full-re-selector (format "\\(%s\\)[ \n{]" full-selector))

  (setq html-buffer (current-buffer))
  (switch-to-buffer-other-window showcss/css-buffer)
  ; save current point so that if search doesn't find
  ; anything, we can return to last point so that the buffer
  ; doesn't scroll to the top
  (setq saved-point (point))
  (goto-char (point-min))
  (if (re-search-forward full-re-selector nil t)
	  (progn
		(showcss/highlight-css-selector (match-beginning 1) (match-end 1)))
	(goto-char saved-point)
	(message "Not found: %s" full-selector))

  (switch-to-buffer-other-window html-buffer))


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
  ; if is a selector:
  (if (or (string= (nth 0 css-values) "class")
		  (string= (nth 0 css-values) "id"))
	  (progn  ; then
		(showcss/display-css)
		(showcss/scroll-to-selector css-values))
	; else:
	;  remove overlays
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
  :lighter " SCss"
  :keymap showcss-map

  ; set the css buffer
  (setq showcss/html-buffer
		(current-buffer))
  (setq showcss/css-buffer
		(showcss/set-css-buffer))

  (if showcss-mode
	  (progn
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

		(ad-activate 'next-line)
		(ad-activate 'previous-line)
		(ad-activate 'right-char)
		(ad-activate 'left-char))

	(showcss/remove-highlights)
	(ad-deactivate 'next-line)
	(ad-deactivate 'previous-line)
	(ad-deactivate 'right-char)
	(ad-deactivate 'left-char)))
