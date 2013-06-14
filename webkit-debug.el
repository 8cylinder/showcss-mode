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

;(wk-connect "/home/sm/projects/show_css/test/index.html")
(defun wk-connect(file)
  "Connect to a running webkit that was started with --remote-debugging-port=9222"
  (let ((url-request-method "GET")
        (url-package-name "webkit-debug.el")
        (url-package-version "0.1")
        (url-http-attempt-keepalives nil)
        (url (url-parse-make-urlobj
              "http" nil nil wk-host wk-port "/json"))
        (file-url (concat "file://" (expand-file-name file))))
    ;; connect to the webkit, get the json, and switch
    ;; to that buffer and read the json
    (with-current-buffer (url-retrieve-synchronously url)
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

            (setq socket (websocket-open
                          (plist-get tab :webSocketDebuggerUrl)
                          :on-open (lambda (websocket)
                                     (message "Websocket opened"))
                          :on-message (function wk-recieve)
                          :on-close (lambda (websocket)
                                      (message "Websocket closed"))))

            (pop-to-buffer "Websocket Output")
            (wk-send socket "DOM.getDocument")
            (websocket-close socket)
            )

        (error "Could not contact remote debugger at %s:%s"
               wk-host
               wk-port)))))



(defun wk-recieve (socket frame)
  (with-temp-buffer
    (insert (format "%s" socket))
    (insert "\n\n")
    (insert (websocket-frame-payload frame))
    (insert "\n\n")
    (insert (format "%s" frame))
    (insert "\n\n")
    (insert "\n\n")
))


(defun* wk-send (socket method &key params)

  (unless (websocket-openp socket)
    (error "Websocket gone, why?: %s" socket))

  ;; build json for browser
  (let ((request (json-encode
                  (list
                   (cons :jsonrpc "2.0")
                   (cons :id 1)
                   (cons :method method)
                   (cons :params params)))))

    (websocket-send-text socket request)
))






(provide 'webkit-debug)
;;; webkit-debug.el ends here
