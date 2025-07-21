(add-hook 'after-make-frame-functions
		  (lambda (frame)
			(select-frame frame) 
			(if (string-equal (buffer-name (current-buffer)) "*scratch*")
				(term))
			)
		  )

(add-hook 'server-after-make-frame-hook
		  (lambda ()
			(if (string-equal (buffer-name (current-buffer)) "*scratch*")
				(term))
			)
		  )

