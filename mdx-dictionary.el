(require 'request)
(require 'shr)
(defun mdx-dictionary-request (word)
  (let* ((url (format "http://localhost:8000/%s" word))
         (response (request url
                            :sync t
                            :parser (lambda ()
                                      (let ((html (decode-coding-string (buffer-string) 'utf-8)))
                                        (erase-buffer)
                                        (insert html)
                                        (libxml-parse-html-region (point-min) (point-max))
                                        ;; (shr-render-region (point-min) (point-max))
                                        ;; (buffer-substring-no-properties (point-min) (point-max))
                                        ))) ))
    (request-response-data response)))

(setq ele (mdx-dictionary-request "hello"))
(ds-get-text (ds-find ele "span" '((class . "phone"))))
(mapcar (lambda (i-node)
          (ds-get-text i-node)) (mapcan (lambda (trs-node)
                                          (ds-findAll trs-node "span" '((class . "i")) nil )) (ds-findAll ele "span" '((class . "trs")))))


