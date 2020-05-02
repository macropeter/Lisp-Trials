;CD-Datenbankprojekt nach Seibel "PCL"
;Dezember 2010

(defvar *db* nil)


(defun make-cd (titel artist rating ripped)
   (list :titel titel 
         :artist artist 
         :rating rating 
         :ripped ripped))

(defun add-record (cd) 
   (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd))) ;kryptische Stringdirektiven

(defun prompt-read (prompt)
  (format *query-io* "~a:" prompt) ;*query-io*: globale Variable für den terminal input stream
  (force-output *query-io*)        ;Problem mi newline bei manchen Dialekten
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd 
    (prompt-read "Titel:")
    (prompt-read "Künstler:")
    (prompt-read "Rating:")
    (y-or-n-p  "gerippt")))            ;gibt j/n aus, erwartet aber y für ja

(defun add-cds ()
   (loop (add-record (prompt-for-cd))  ;quasi repeat-Schleife
         (if (not (y-or-n-p "Noch eine CD eingeben?")) (return))
))

(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))


(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))


