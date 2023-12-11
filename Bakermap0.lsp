;;;eindimensionale Bakermap
;;;dynamisches System, wie von Gisin vorgeschlagen
;; April 2021

					; zeigt, dass CLisp standardmäßig mit 32bit-Realzahlen rechnet
;; rechnet man mit Brüchen, kann man die ganze Kapazität von Lisp präsentieren

(defun mod1 (x)
  "modulo 1 rechnen"
  (multiple-value-bind (ganz teil) (floor x)
  teil))

(defun baker (x)
  "einfachste Bäckertrafo: ausrollen und falten"
    (if (< x 0.5)
        (* 2 x)
      (- 2 (* 2 x))))


(defun baker1 (i x)
  "iterierte Bäckertrafo"
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

(defun bakerprocess (startx maxz)
  "Iterierte Bäckertrafo, am besten mit einem Bruch starten"
  (labels ((baker (i x)
	     (when (<= i maxz)
	       (format t "~d , ~f~%" i x)
	       (if (>= x 0.5)
		   (baker (1+ i)
			  (- (* 2 x) 1))
		   (baker (1+ i)
			  (* 2 x))))
	     )
	   )
    (baker 1 startx) ;zufällige double-float-Zahl: (random 0.34d0) oder einen Bruch
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


