
(defun directory-dirs (dir)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
  (dirs '())
	(files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
	(let ((file (concat (file-name-as-directory dir) file)))
	  (when (file-directory-p file)
	    (setq dirs (append (cons file
				     (directory-dirs file))
			       dirs))))))
    dirs))





(defun my-walk-path (path action)
  "walk DIR executing ACTION with (path)"
;;  (unless (file-exists-p path)
;;    (error "Path `%s' does not exist" path))
  (if (file-exists-p path)
      (progn
	(funcall action path)
	(if (file-directory-p path)
	    
            (dolist (entry (directory-files path nil nil t))				     
	      (unless (member entry '("." ".."))					     
	    	(my-walk-path (concat (file-name-as-directory path) entry) action)	     
	    	)								
	      )									     
            
	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; (mapcar (lambda (entry) (unless (member entry '("." ".."))					     ;;
	    ;; 			      (my-walk-path (concat (file-name-as-directory path) entry) action)))	     ;;
	    ;; 	    (directory-files path nil nil t))								     ;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ))
    )
  )


(defun my-filter (condp lst)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst))
)


(defun walk-path (dir action)
  "walk DIR executing ACTION with (dir file)"
  (cond ((file-directory-p dir)
	 (or (char-equal ?/ (aref dir(1- (length dir))))
	     (setq dir (file-name-as-directory dir)))
	 (let ((lst (directory-files dir nil nil t))
	       fullname file)
	   (while lst
	     (setq file (car lst))
	     (setq lst (cdr lst))
	     (cond ((member file '("." "..")))
		   (t
		    (and (funcall action dir file)
			 (setq fullname (concat dir file))
			 (file-directory-p fullname)
			 (walk-path fullname action)))))))
	(t
	 (funcall action
		  (file-name-directory dir)
		  (file-name-nondirectory dir)))))







;; (let ((default-directory "."))
;;   (dolist (entry (directory-dirs "z:\\tmp"))
;;     (message entry))
;;   )



;; (concat (file-name-as-directory dirfile) relfile)


(defun walk-path-visitor (dir file)
  "Called by walk-path for each file found"
  (message (concat (file-name-as-directory dir) file)))

;; (walk-path "~/" 'walk-path-visitor)

(defun visitor (path)
  "Called by walk-path for each file found"
  (message (concat path "\n")))

(defun matches-path (pattern path)
  (if (string-match pattern path)
      (message path))
)



;; (my-walk-path "z:\\backup" (lambda (path) (matches-path "x" path)));;




(defun my-find-grep (path pattern)
 ;; (interactive)
 ;; (let ((path (ido-read-directory-name "Path: ")))
  (interactive "sPath: \nsPattern: \n")
  (my-walk-path path(lambda (path) (matches-path pattern path)))
)

