;;; buffer-combine-mode.el


;;; Code:

(require 'cl-lib)
(require 's)


(defgroup buffer-combine nil
  "Customize buffer-combine"
  :prefix "buffer-combine"
  :group 'Text)

(defface buffer-combine/region-face
  '((t (:background "grey50")))
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
  (let ((buffers-data '()))
    ;;for each file and its fragment positions:
    (dolist (filelist data)
      (let ((buffer (bc/load-file (car filelist) hidden)))
        ;;for each fragment position:
        (setq buffers-data
              (cons (bc/mark-fragments-in-source buffer (cdr filelist))
                    buffers-data))))

    (bc/build-display buffers-data)
))


(defun bc/load-file (file hidden)
  "Load the files from disk and if hidden is t, rename them with a
space in front of the buffer title"
  (let ((buffer file))
    ;; if buffer is not a buffer then it is probably
    ;; a string (file location).  Then convert it to
    ;; a buffer.
    (unless (bufferp buffer)
      (setq buffer (find-file-noselect file)))
    (set-buffer buffer)
    ;; if file is to be hidden, add a space to the
    ;; begining of the buffer name
    (if hidden
        (rename-buffer (concat " " (s-trim-left (buffer-name))))
      ;; if file is *not* to be hidden but it is already,
      ;; unhide it by removing the space at the begining.
      (if (s-starts-with? " " (buffer-name))
          (rename-buffer (s-trim-left (buffer-name)))))

    ;; return: <buffer handle>
    buffer))


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
              (nth 0 fragment)    ;start
              (nth 1 fragment)    ;end
              "fragment name?")
             buffer-overlay-list)))
    (reverse buffer-overlay-list)))


(defun bc/mark-fragment-in-source (buffer start end name)
  "Mark a fragments in a buffer with an overlay"
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face 'buffer-combine/region-face)
    (overlay-put ov 'before-string
                 "\nChanges made here will be overwritten:\n")
    (overlay-put ov 'name name)

    ;; return <ov>
    ov))


(defun bc/build-display(buffers-data)
  "Build the display for each fragment"
  (set-buffer bc/this-buffer)
  (dolist (file-and-overlays buffers-data)
    (let ((buf (car file-and-overlays)))
      (insert (format "\n/* %s */\n\n" (buffer-file-name buf)))
      (dolist (source-ov (cdr file-and-overlays))
        (let* ((source-start (overlay-start source-ov))
               (source-end (overlay-end source-ov))
               (display-length (- source-end source-start)))
          (insert-buffer-substring-no-properties
           buf
           (overlay-start source-ov)
           (overlay-end source-ov))
          (let ((display-ov (make-overlay (point) (- (point) display-length) nil nil nil)))
            (overlay-put display-ov 'before-string "\n")
            (overlay-put display-ov 'modification-hooks '(send-back-to-source))
            (overlay-put display-ov 'source-overlay source-ov)
            (overlay-put display-ov 'face 'buffer-combine/region-face))
          (insert "\n")
)))))


(defun send-back-to-source(ov &optional flag &rest rv)
  ""
  ;(message (format "%s %s %s -- %s" ov flag rv  (point)))
  (message (format "%s %s" ov (overlay-get ov 'source-overlay)))
  (let* ((source-ov (overlay-get ov 'source-overlay))
         (source-start (overlay-start source-ov))
         (source-end (overlay-end source-ov))
         (display-start (overlay-start ov))
         (display-end (overlay-end ov))
         (content (buffer-substring-no-properties display-start display-end)))
    (set-buffer (overlay-buffer source-ov))
    (delete-region source-start source-end)
    (insert content))
)





(defvar buffer-combine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "" 'bc/save-all-buffers)
    map)
  "some documentation")

;; derive from css-mode, sass-mode?
(define-derived-mode buffer-combine-mode css-mode "Combine"
  "Display fragments from other buffers"
  :group 'buffer-combine

  (setq bc/this-buffer (current-buffer))
)


(provide 'buffer-combine)

;;; buffer-combine-mode.el ends here
