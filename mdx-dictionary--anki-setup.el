(require 'f)

(defun mdx-dictionary--save-to-anki (content)
  (let* ((word (word-at-point))
         (sentence (replace-regexp-in-string "[\r\n]+" " " (or (sentence-at-point)
                                                               (thing-at-point 'line)))) ;去掉句子中的断行
         (sentence (replace-regexp-in-string (regexp-quote word)
                                             (lambda (word)
                                               (format "<b>%s</b>" word))
                                             sentence)) ;高亮句子中的单词
         (content (replace-regexp-in-string "[\r\n]+" "<br>" content)))
    (f-append (format "%s|%s|%s\n" word content sentence) 'utf-8  "~/mdx-dictionary-for-anki.txt")))

(add-hook 'mdx-dictionary-display-before-functions #'mdx-dictionary--save-to-anki)

