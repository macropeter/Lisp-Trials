; Das Ziegenproblem als OOP-Beispiel
; Oktober 2014

(defclass tuer ()
  ((treffer :accessor treffer
	    :initform nil
	    :documentation "Ziege (nil) oder Auto")
   (offen   :accessor offen
	    :initform nil
	    :documentation "Tür geöffnet oder nicht (nil)")))

(setq *tueren* (make-array 3 :element-type 'tuer)) ; Array mit 3 Türen

(loop for i below 3 do (setf (aref *tueren* i)  
			     (make-instance 'tuer))) ; Objekte initialisieren


(setf (treffer (aref *tueren* (random 3))) T) ; eine zufällige Tür als Treffer auswählen

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt) 
  (read-line *query-io*))

(defun rate ()
       (null (treffer (aref *tueren* (prompt-read "0, 1 oder 2:")))))
