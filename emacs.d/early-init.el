;; (defun start-term ()
;;   (if (string-equal (buffer-name (current-buffer)) "*scratch*")
;; 	  (progn
;; 		(term "/bin/bash")
;; 		;; (term-line-mode)
;; 		(rename-uniquely)
;; 		)))

;; (add-hook 'after-make-frame-functions
;; 		  (lambda (frame)
;; 			(select-frame frame)
;; 			(start-term)))


;; (add-hook 'server-after-make-frame-hook 'start-term)

