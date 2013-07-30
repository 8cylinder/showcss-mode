;;; webkit-debug.el --- Connect to webkit to get css info about a webpage

;; Copyright (C) 2013 Sheldon McGrandle

;; Author: Sheldon McGrandle <developer@rednemesis.com>
;; Version: 0.1
;; Created:  June 11, 2013
;; Keywords: hypermedia
;; URL: https://github.com/smmcg/showcss-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;https://www.webkit.org/blog/1875/announcing-remote-debugging-protocol-v1-0/
;;http://trac.webkit.org/export/129139/trunk/Source/WebCore/inspector/Inspector.json
;;https://developers.google.com/chrome-developer-tools/docs/protocol/1.0/

(require 'json)
(require 'websocket)
(require 'url-parse)
(require 'cl)

(defvar wk-port 9222
  "Default port for remote-debug google-chrome")
(defvar wk-host "127.0.0.1"
  "Default host for remote-debug google-chrome")
(defvar wk-source-file "file:///"
  "File to debug")  ;FIXME: remove later

(defvar wk-debug-buffer "Webkit debug buffer")
(defvar wk-socket nil)


(defun wk-get-data()
  (wk-send "Page.enable" :id 1)
  (wk-send "CSS.enable" :id 2)

  ;(wk-send "Page.reload" :id 1 :params (list :ignoreCache t))
  ;;(wk-send "DOM.getDocument" :id 2)
  ;(wk-send "DOM.requestChildNodes" :id 21 :params (list :NodeId 46))
  ;(wk-send "CSS.getAllStyleSheets" :id 3) x
  ;(wk-send "DOM.performSearch" :id 4 :params (list :query "showcss"))
  ;(wk-send "Page.getResourceTree" :id 5)
)


;(wk-connect "/home/sm/projects/show_css/test/index.html")
(defun wk-connect(file)
  "Connect to a running webkit that was started with --remote-debugging-port=9222"
  (let* ((url-request-method "GET")
         (url-package-name "webkit-debug.el")
         (url-package-version "0.1")
         (url-http-attempt-keepalives nil)
         (url (url-parse-make-urlobj
               "http" nil nil wk-host wk-port "/json"))
         (file-url (concat "file://" (expand-file-name file)))
         (url-retrieve-buffer (url-retrieve-synchronously url)))
    ;; connect to the webkit, get the json, and switch
    ;; to that buffer and read the json
    (with-current-buffer url-retrieve-buffer
      (goto-char 0)
      (if (and (looking-at "HTTP/1\\.. 200")
               (re-search-forward "\n\n" nil t))
          ;; read json into 'tabs
          (let* ((tabs (let ((json-array-type 'list)
                             (json-object-type 'plist))
                         (json-read)))
                 (tab)
                 (socket))
            ;; extract the chrome tab we're interested in
            (dolist (iter-tab tabs)
              (if (string= (plist-get iter-tab :url) file-url)
                  (setq tab iter-tab)))
            (if (not tab)
                (error "No Chrome tab found for %s" file-url))

            (setq wk-socket (websocket-open
                             (plist-get tab :webSocketDebuggerUrl)
                             :on-open (lambda (websocket)
                                        (message "Websocket opened"))
                             :on-message (function wk-recieve)
                             :on-close (lambda (websocket)
                                      (message "Websocket closed")))))

        (error "Could not contact remote debugger at %s:%s"
               wk-host
               wk-port)))
    ;(kill-buffer url-retrieve-buffer)
    ))



(defun wk-recieve (socket frame)
  ;(switch-to-buffer wk-debug-buffer)
  (with-current-buffer wk-debug-buffer
    (goto-char (point-min))
    ;(insert (pp-to-string (json-read-from-string (websocket-frame-payload frame))))
    (insert (pp-to-string (websocket-frame-payload frame)))

    ;;(let ((action-id (cdr (car (json-read-from-string (websocket-frame-payload frame))))))
    ;;  (if (= action-id 2)
    ;;      (let ((node-id (cdr (nth 5 (cdr (cdr (elt (cdr (nth 3 (cdr (car (cdr (car (cdr (json-read-from-string (websocket-frame-payload frame))))))))) 0)))))))
    ;;        (wk-send "DOM.requestChildNodes" :id 99999 :params (list :NodeId node-id)))
    ;;    ))

    (insert "\n\n")
    ))


(defun* wk-send (method &key params id)
  (unless (websocket-openp wk-socket)
    (wk-connect "/home/sm/projects/show_css/test/index.html"))

  ;; build json for browser
  (let ((request (json-encode
                  (list
                   (cons :jsonrpc "2.0")
                   (cons :id id)
                   (cons :method method)
                   (cons :params params)))))

    (websocket-send-text wk-socket request)
  ))






(provide 'webkit-debug)
;;; webkit-debug.el ends here


;;(defun wss-recieve (websocket frame)
;;  ;(pop-to-buffer "Test websocket output")
;;  (with-current-buffer "Test websocket output"
;;    ;(delete-region (point-min) (point-max))
;;    (insert
;;     (pp-to-string
;;      (json-read-from-string (websocket-frame-payload frame))))
;;    ;(websocket-close websocket)
;;  ))
;;
;;(setq wss-socket (websocket-open
;;                  "ws://127.0.0.1:9222/devtools/page/3_1"
;;                  :on-message (function wss-recieve)))
;;
;;
;;
;;(progn
;;  (let ((js-doc (json-encode
;;                 (list
;;                  (cons :jsonrpc "2.0")
;;                  (cons :id 12)
;;                  (cons :method "DOM.getDocument")
;;                  (cons :params nil)
;;                    )))
;;        (js (json-encode
;;             (list
;;              (cons :jsonrpc "2.0")
;;              (cons :id 12)
;;              (cons :method "DOM.requestNode")
;;              (cons :params
;;                     (list :objectId "136"))
;;                    ))))
;;    (print js)
;;    (websocket-send-text wss-socket js-doc)
;;    (websocket-send-text wss-socket js)))
