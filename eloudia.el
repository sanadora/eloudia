;;; eloudia.el --- interfaz para CloudIA

;;; Commentary:
;; 

(require 'cl-lib)
 
;;; Code:

(cl-defstruct cluster name type nodes)
(cl-defstruct pool name cluster)
(cl-defstruct endpoint name domaingroup path pools)

;; obtener data de cluster(C) / pool(P) / endpoint(E): parsear la salida del ssh
;; pasar data a un modelo que representen clusters, etc.

(defun length-of-lengthiest-section (&rest records-section)
  "Return the max length, given a list of the same section of some records."
  (cl-reduce 'max
	     (cl-mapcar 'length records-section)))

(defun max-records-lengths (records)
  "Generate a list with the max length of each section of the RECORDS.
It traverses the first section of each record and  the length of the largest.
Then with the second, third, and so on... and returns a record with each section's result."
  (apply 'cl-mapcar 'length-of-lengthiest-section records))

(defun format-record (record format-lengths)
  "Generate a RECORD with its strings stretched to match FORMAT-LENGTHS."
  (cl-mapcar (lambda (rec-section section-length)
	       (format (format "%%-%ds" section-length) rec-section))
	     record
	     format-lengths))

(defun format-records (records)
  "Generate a list of RECORDS with normalized lengths.
It determines the max length of a record section, and adds aditional
space to the other records respective section."
  (let* ((record-of-max-lengths
	  (max-records-lengths records))
	 result)
    (dolist (rec records result)
      (setq result (cons (format-record rec record-of-max-lengths)
			 result)))))

(defun cluster->record (cluster)
  "Generate a record form a CLUSTER."
  (vector (cluster-name cluster)
	  (cluster-type cluster)
	  (cluster-nodes cluster)))

(defun format-clusters-info (clusters)
  (format-records (mapcar 'cluster->record clusters)))

(defun record->string (record)
  (string-join record "\t"))

(defun records->string (records)
  (string-join (mapcar 'record->string records)
	       "\n"))

(defun prepend-ids (records)
  "It prepends a numeric ID for every RECORD."
  (let (result
	(num 0))
    (while (not (null records))
      (setq result (cons (list num (car records))
			 result))
      (setq num (1+ num))
      (setq records (cdr records)))
    result))


(setq clusters (list (make-cluster :name "ken" :type "ubuntu plux" :nodes "2")
		     (make-cluster :name "lotso" :type "ubunty" :nodes "5")))

(define-derived-mode eloudia-mode tabulated-list-mode "Eloudia"
  "Major mode for managing Cloudia"
  (setq clusters-tabulated-list-format [("Name" 20 t)
					("Type" 20 t)
					("#nodes" 5 t)])
  (setq tabulated-list-format clusters-tabulated-list-format)
  (add-hook 'tabulated-list-revert-hook 'list-clusters--refresh nil t)
  (tabulated-list-init-header))

(defun list-clusters--refresh ()
  "Prepares the data (entries) to be displayed."
  (setq tabulated-list-entries
	(prepend-ids (mapcar (function cluster->record) clusters))))

;;;;;;;;;;;;;;;;;;;;;
;; listing command ;;
;;;;;;;;;;;;;;;;;;;;;
(defun list-clusters ()
  "Creates (or switches to) a buffer with a listing of the clusters."
  (interactive)
  (switch-to-buffer (get-buffer-create "*eloudia-listing*"))
  (eloudia-mode)
  (list-clusters--refresh)
  (tabulated-list-print t t))

	


(provide 'eloudia)

;;; eloudia.el ends here
