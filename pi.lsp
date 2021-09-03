;;;SICP 1.3.1

(defun pireihe (i n)
  (if (> i n)
      0
      (+ (* (/ 1.0 i)
	    (/ 1.0 (+ i 2)))
	 (pireihe (+ i 4) n))))


(defun faltung (fkt i n)
  (if (> i n)
      0
      (+ (funcall fkt i)
	 (faltung fkt (1+ i) n))))


(defun pi-term (n)
  (* (/ 1.0 n)
     (/ 1.0 (+ n 2))))


(defun falt-summe (fkt i naechst n)
  (if (> i n)
      0
      (+ (funcall fkt i)
	 (falt-summe fkt (funcall naechst i) naechst n))))

;; (falt-summe #'pi-term 1 (lambda (x)(+ x 4)) 10)

(defun pi-summe (a b)
  (flet ((pi-next (x) (+ x 4.0))
	 (pi-term1 (x) (* (/ 1.0 x)
			  (/ 1.0 (+ 2 x)))))
	(falt-summe #'pi-term1 a #'pi-next b)))

;; (* 8 (pi-summe 1 50))
