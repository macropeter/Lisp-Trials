;;;eindimensionale Bakermap
;;;dynamisches System, wie von Gisin vorgeschlagen
;; April 2021

; zeigt, dass CLisp standardmäßig mit 32bit-Realzahlen rechnet

(defun baker1 (i x)
  (when (<= i 100)
    (princ i)
    (princ ": ")
    (princ x)
    (terpri)
    (if (>= x 0.5)
	(baker1 (1+ i)
		(- (* 2 x) 1))
	(baker1 (1+ i)
		(* 2 x)))))

;Ausgabe mit format
(defun bakerf (i maxz x)
  (when (<= i maxz)
    (format t "~d : ~f~%" i x)
    (if (>= x 0.5)
	(bakerf (1+ i)
		maxz
		(- (* 2 x) 1))
	(bakerf (1+ i)
		maxz
		(* 2 x)))))

(defun bakerprocess (maxz)
  (labels ((baker (i x)
	     (when (<= i maxz)
	       (format t "~d : ~f~%" i x)
	       (if (>= x 0.5)
		   (baker (1+ i)
			  (- (* 2 x) 1))
		   (baker (1+ i)
			  (* 2 x))))))
    (baker 1 (random 1.0d0)) ; mit d0 zufällige double-float-Zahl erstellen
    )
  )

(defun bakerziffer (i maxz x)
  (let ((xx (floor (* 2 x)))) ; nur für die Ausgabe der Ziffer 0 oder 1
  (when (<= i maxz)
    (format t "~d : ~f :~T ~d~%" i x xx)
    (if (>= x 0.5)
	(bakerziffer (1+ i)
		     maxz
		     (- (* 2 x) 1))
	(bakerziffer (1+ i)
		     maxz
		     (* 2 x))))))


