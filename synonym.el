
(defun make-cache ()
  (make-hash-table :test 'equal))

(defun* cache-get (key default-fn cache &key (filename nil))
  (let ((val (gethash key cache)))
    (if val
        val
      (let ((val (puthash key (funcall default-fn key) cache)))
        (when filename
            (cache-save filename cache))
        val))))

(defun cache-save (filename cache)
  (with-temp-buffer
    (insert (with-output-to-string
              (princ cache)))
    (write-region (point-min) (point-max) filename)))

(defun cache-load (filename)
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/synonym.cache")
    (car (read-from-string (buffer-substring-no-properties
                            (point-min) (point-max))))))

(unless (boundp '*synonym-api-key*)
  (error "Invalid api key."))

(defun synonym-get-buffer (word)
  (url-retrieve-synchronously
   (concat "http://words.bighugelabs.com/api/2/"
           *synonym-api-key* "/" word "/")))

(defun synonym-handle-line ()
  (kill-word 1)
  (delete-char 1)
  (setq syn? (thing-at-point 'word))
  (if (string= syn? "syn")
      (progn
        (kill-word 1)
        (delete-char 1)
        (add-to-list 'synonym-synonyms
                     (buffer-substring-no-properties
                      (point) (line-end-position)))))
  (kill-line 2))

(defun synonym-fetch-synonyms (word)
  (setq synonym-synonyms '())
  (save-excursion
    (with-current-buffer (synonym-get-buffer word)
      (goto-char (point-min))
      (while (not (= (point-min) (point-max)))
        (let ((l (synonym-handle-line)))
          (if l
              (setq synonym-synonyms l))))))
  (reverse synonym-synonyms))

(if (file-exists-p "~/.emacs.d/synonym.cache")
    (setq synonym-cache (cache-load "~/.emacs.d/synonym.cache"))
  (setq synonym-cache (make-cache)))

(defun synonym-get-synonyms (word)
  (cache-get word 'synonym-fetch-synonyms synonym-cache
             :filename "~/.emacs.d/synonym.cache"))

(defun synonym-show-synonyms ()
  (interactive)
  (backward-word 1)
  (insert (popup-menu* (synonym-get-synonyms (thing-at-point 'word))
                       :point (point)))
  (kill-word 1))

(define-key global-map (kbd "C-x t") 'synonym-show-synonyms)

(provide 'synonym)
