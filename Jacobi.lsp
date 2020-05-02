;;Der Solovay-Strassen-Test für Primzahlen:
;;Ein MC-Verfahren!



(defun qTest (n x m)
  "Test ob n ein qu Rest von x mod m ist"
  (eq (mod (* n n) m) x))


(defun test2 (x m)
  "gibt List aller quadratischen Rest von x mod m zurück"
    (loop for i from 2 to (1- m)
	     when (qTest i x m)
	     collect i))


(defun exptm (x n m)
  "ganzahlige Potenz modulo m: x hoch n mod m"
  (if (zerop n) 1
    (mod (* x
 	    (exptm x
	           (1- n)
	           m))
	  m)))

(defun pseudoprim (x n)
    "berechnet b=x^(n-1)/2 mod n"
    (exptm x
	   (/ (1- n)
	      2)
	   n))
    

