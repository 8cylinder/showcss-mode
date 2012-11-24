;;

(defgroup showcss nil
  "Customize showcss"
  :group 'Text)

(defface showcss/css-face
  '((t :background "grey"))
  "Highlight the matched selector in the css file"
  :group 'showcss)

(defface showcss/html-face
  '((t :background "grey"))
  "Highlight the selector the cursor is in"
  :group 'showcss)

; add customize to asocciate html files with a css file
; so the string doesn't need to be in the html file.

; ----------------------------------------------------

(defvar showcss/last-css-overlay (make-overlay 0 0)
  "this is the last overlay set in the css file")
(defvar showcss/last-html-overlay (make-overlay 0 0)
  "this is the last overlay set in the html file")
(defvar showcss/css-buffer nil
  "The buffer that contains the css file")

; ----------------------------------------------------

(defun showcss/get_css_file()
  "Find the name of the css file using this regex:
<!-- show-css: \\(.*\\) -->
Eg:
<!-- show-css: /home/sm/projects/some project/site/css/main.css -->
"
  (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "<!-- show-css: \\(.*\\) -->" nil t)
		()
	  (error "\"<!-- show-css: ... -->\" does not exist in html file")))

  ; RETURN css file name
  (match-string 1))

(defun showcss/what-am-i()
  "What is the cursor on?  Should return class,
id, or nil and the class name or id name"
  (setq saved-point (point))

  (re-search-backward "[ \t]" nil t)
  (re-search-forward " \\(id\\|class\\)=\"\\(.*?\\)\"" nil nil 1)
  ; is the saved-point between (match-beginning 0) and (match-end 0)?
  (if (and (> saved-point (match-beginning 0))
		   (< saved-point (match-end 0)))
   	  (showcss/highlight-html-selector (match-beginning 0) (match-end 0))
	(showcss/remove-highlights))

  (setq selector-type
   		(substring-no-properties (match-string 1)))
  (setq selector-name
   		(substring-no-properties (match-string 2)))
  (goto-char saved-point)

  ; RETURN the selector type and name
  (list selector-type selector-name)
)

(defun showcss/show_source (css-file)
  "Display the css buffer"
  ; TODO: check if css-file exists
  (setq css-buffer (find-file-noselect css-file))
  (display-buffer css-buffer)
  (setq showcss/css-buffer css-buffer)

  ; RETURN buffer
  css-buffer)

(defun showcss/scroll-to-selector (css-buffer css-values)
  "Scroll the css file to show the selector"
  (setq selector-type (nth 0 css-values))
  (setq selector-name (nth 1 css-values))
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

  (message "looking for: %s" full-selector)

  (setq html-buffer (current-buffer))
  (switch-to-buffer-other-window css-buffer)
  ; save current point so that if search doesn't find
  ; anything, we can return to last point so that the buffer
  ; doesn't scroll to the top
  (setq saved-point (point))
  (goto-char (point-min))
  (if (search-forward full-selector nil t)
	  (progn
		;(message "%s" (match-beginning 0) (match-end 0))
		(showcss/highlight-selector (match-beginning 0) (match-end 0))
		)
	(goto-char saved-point))


  (switch-to-buffer-other-window html-buffer)
)

(defun showcss/highlight-selector (start end)
  "Highlight the matched selector"
  (delete-overlay showcss/last-css-overlay)
  (setq ov (make-overlay start end))
  (overlay-put ov 'face 'showcss/css-face)
  (setq showcss/last-css-overlay ov))

(defun showcss/highlight-html-selector (start end)
  "Highlight the current selector in the html file"
  (showcss/remove-highlights)
  (setq ov (make-overlay start end))
  (overlay-put ov 'face 'showcss/html-face)
  (setq showcss/last-html-overlay ov))

(defun showcss/remove-highlights()
  "remove the last highlight from the html buffer"
  (delete-overlay showcss/last-html-overlay)
  (setq ov nil)
  ;(set-buffer showcss/css-buffer)
  ;(delete-overlay showcss/last-css-overlay)
)

(defun showcss/main()
  (interactive)
  ""
  (setq css-values (showcss/what-am-i))

  ; if is a selector:
  (if (or (string= (nth 0 css-values) "class")
		  (string= (nth 0 css-values) "id"))
	  (showcss/scroll-to-selector showcss/css-buffer css-values) ; then
	() ; else: remove overlays


)

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
  (setq showcss/css-buffer
		(showcss/show_source
		 (showcss/get_css_file)))

)
