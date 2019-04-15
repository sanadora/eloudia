(require 'cl-lib)
 
(cl-defstruct cluster name type nodes)
(cl-defstruct pool name cluster)
(cl-defstruct endpoint name domaingroup path pools)

;; obtener data de cluster (C) /pool (P) /endpoint (E): parsear la salida del ssh o el json de un curl
;; pasar data a un modelo que representen clusters, etc.
;; DONE generar un modelo imprimible (tener en cuenta cosas como el nombre mas largo, tamanio de la pantalla, etc)
;; mostrar las estructuras en un buffer

(defun max-length (lengths)
  (cl-reduce (function max) lengths))
(defun string-lengths (strings)
  (cl-mapcar (function length) strings))

(defun max-records-lengths (records)
  "Generates a list with the max lengths of each record section of RECORDS.
It traverses the first section of each record and computes the length of the largest.
Then with the second, third, and so on... and returns a record with each section's result."
  (apply (function cl-mapcar)
	 (cons (lambda (&rest records-section)
		 (max-length (string-lengths records-section)))
	       records)))

(defun format-record (record format-lengths)
  "Generates a record with its strings stretched to match FORMAT-LENGTH."
  (cl-mapcar (lambda (rec-section section-length)
	       (format (format "%%-%ds" section-length) rec-section))
	     record
	     format-lengths))

(defun format-records (records)
  "Generates a list of records with normalized lengths.
It determines the max length of a record section, and adds aditional
space to the other records respective section."
  (let* ((record-of-max-lengths
	  (max-records-lengths records))
	 result)
    (dolist (rec records result)
      (setq result (cons (format-record rec record-of-max-lengths)
			 result)))))

(defun cluster->record (cluster)
  "Generates a record form a cluster."
  (list (cluster-name cluster)
	(cluster-type cluster)))

(defun format-clusters-info (clusters)
  (format-records (mapcar (function cluster->record) clusters)))


(defun record->string (record)
  (string-join record "\t"))

(defun records->string (records)
  (string-join (mapcar (function record->string) records)
	       "\n"))

(setq clusters (list (make-cluster :name "ken" :type "ubuntuyyy" :nodes 2)
		     (make-cluster :name "lotso" :type "ubunty" :nodes 5)))

(with-current-buffer "*scratch*"
  (insert (records->string (format-clusters-info clusters))))



(setq tabulated-list-entries
      '((1 ("Ken" "Ubuntu" 5))
	(2 ("Lotso" "Ubuntu plus" 2))))

(define-derived-mode eloudia-mode tabulated-list-mode "Eloudia"
  "Major mode for managing Cloudia"
  (setq clusters-tabulated-list-format
      ['("CL Name" 20 t)
       '("CL Type" 20 t)
       '("CL #nodes" 5 t)])
  (setq tabulated-list-format clusters-tabulated-list-format)
  (tabulated-list-init-header))
  
  
	

