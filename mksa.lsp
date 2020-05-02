;;;MKSA = Meter kg Sek Ampere
;;; Grunddeinheiten der Mechanik
;;; nach Krusenotto Anwendungsbeispiel 1


; Datenstruktur: (Wert Meter kg sec Ampere)
(defun u* (a b)
  "Multiplikation im mksa-System"
  (cons (* (car a) (car b))
	(mapcar '+ (cdr a) (cdr b))))

(defun u+ (a b)
  "Addition im mksa-System, nur bei
gleichen Einheiten möglich"
  (if (equal (cdr a)
	     (cdr b))
      (cons (+ (car a)
	       (car b))
	    (cdr a))
      (error "Inkompatible Einheiten")))


