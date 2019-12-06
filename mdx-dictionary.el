;;; mdx-dictionary.el --- MDX Dictionary interface for Emacs  -*- lexical-binding: t; -*-

;; Copyright © 2017 DarkSun

;; Author: DarkSun <lujun9972@gmail.com>
;; URL: https://github.com/lujun9972/mdx-dictionary.el
;; Package-Requires: ((popup "0.5.0")(request "0.2.0"))
;; Version: 0.1
;; Keywords: convenience, Chinese, dictionary

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
;; A simple MDX Dictionary interface for Emacs. It need mdx-server(https://github.com/ninja33/mdx-server) to read MDX/MDD dictionary data
;;
(require 'request)
(require 'shr)
(require 'thingatpt)
(require 'popup)

(defgroup mdx-dictionary nil
  "dictionary based on mdx-server"
  :prefix "mdx-dictionary-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/lujun9972/mdx-dictionary.el"))

;; (defcustom mdx-dictionary-server-file "/home/lujun9972/github/mdx-dictionary/mdx-server/mdx_server.py"
;;   "mdx-server execution file"
;;   :type 'file)

;;;###autoload
(defcustom mdx-dictionary-server-file (concat (file-name-directory (or load-file-name byte-compile-current-buffer buffer-file-name)) "mdx-server/mdx_server.py")
  "mdx-server execution file"
  :type '(file :must-match t))

(defcustom mdx-dictionary-python "python3"
  "python used to start mdx-server"
  :type 'string)

(defcustom mdx-dictionary-server-args nil
  "Args to launch mdx dictionary server"
  :type '(repeat string))

(defvar mdx-dictionary-server-process nil)

;;;###autoload
(defun mdx-dictionary-start-server (mdx-file)
  (interactive "fPlease choose a mdx-file:")
  (mdx-dictionary-stop-server)
  (let ((args `(,@mdx-dictionary-server-args ,mdx-file)))
    (setq mdx-dictionary-server-process (apply #'start-process "mdx-dictionary-server" "*mdx-dictionary-server*" mdx-dictionary-python mdx-dictionary-server-file args))))

;;;###autoload
(defun mdx-dictionary-stop-server ()
  (interactive)
  (when mdx-dictionary-server-process
    (delete-process mdx-dictionary-server-process)
    (setq mdx-dictionary-server-process nil)))

;;;###autoload
(defun mdx-dictionary-request (word)
  (let* ((url (format "http://localhost:8000/%s" (url-hexify-string word)))
         (response (request url
                            :sync t
                            :parser (lambda ()
                                      (let ((html (decode-coding-string (buffer-string) 'utf-8)))
                                        (erase-buffer)
                                        (insert html)
                                        (libxml-parse-html-region (point-min) (point-max))
                                        ))) ))
    (request-response-data response)))

(defcustom mdx-dictionary-parser #'mdx-dictionary--default-parser
  "function used to format dom into string")


(defun mdx-dictionary--default-parser (word dom)
  "Default parser used to parse `DOM'"
  (let ((expression word)
        (glossary (with-temp-buffer
                    (shr-insert-document dom)
                    (buffer-substring-no-properties (point-min) (point-max)))))
    `((expression . ,expression)
      (glossary ,glossary))))

(defun mdx-dictionary--21世纪大英汉词典-parser (word dom)
  "The function used to parser 21世纪大英汉词典"
  (let ((expression (dom-texts (car (dom-by-class dom "^return-phrase$"))))
        (phonetic (dom-texts (car (dom-by-class dom "^phone$"))))
        (glossary (mapcar (lambda (dom)
                            (nth 2 (dom-strings dom)))
                          (dom-by-class dom "^tr$"))))
    `((expression . ,expression)
      (us-phonetic . ,phonetic)
      (glossary . ,glossary))))

(defun mdx-dictionary-searcher (word)
  "Function used to search `WORD' meanings"
  (let* ((response (mdx-dictionary-request word)))
    (when response
      (funcall mdx-dictionary-parser word response))))

(defun mdx-dictionary-query (&optional word)
  (interactive)
  (let* ((word (or word
                   (and (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                   (word-at-point))) 
         (content (mdx-dictionary-searcher word)))
    (if content
        (let ((expression (cdr (assoc 'expression content)))
              (us-phonetic (cdr (assoc 'us-phonetic content)))
              (uk-phonetic (cdr (assoc 'uk-phonetic content)))
              (glossary (mapconcat #'identity (cdr (assoc 'glossary content)) "\n")))
          (popup-tip (format "%s(%s)\n%s"
                             expression
                             (or us-phonetic uk-phonetic "")
                             glossary)))
      (setq word (read-string "该单词可能是变体,请输入词源(按C-g退出): " word))
      (mdx-dictionary-query word))))




(provide 'mdx-dictionary)
;; mdx-dictionary.el ends here
