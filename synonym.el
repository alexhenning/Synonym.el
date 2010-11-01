
(setq *synonym-api-key* "")

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

(defun synonym-get-synonyms (word)
  (setq synonym-synonyms '())
  (save-excursion
    (with-current-buffer (synonym-get-buffer word)
      (goto-char (point-min))
      (while (not (= (point-min) (point-max)))
        (let ((l (synonym-handle-line)))
          (if l
              (setq synonym-synonyms l))))))
  (reverse synonym-synonyms))

(defun synonym-show-synonyms ()
  (interactive)
  (backward-word 1)
  (insert (popup-menu* (synonym-get-synonyms (thing-at-point 'word))
                       :point (point)))
  (kill-word 1))

(define-key global-map (kbd "C-x t") 'synonym-show-synonyms)
