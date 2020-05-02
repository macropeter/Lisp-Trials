;Fibonaccizahlen, verschiedene Varianten
;letzte Bearbeitung: April 2011

;klassisches, sehr ineffektives Verfahren:
(defun fibo (x)
   (cond  ((= x 0) 0)
          ((= x 1) 1)
          (T (+ (fibo (- x 1)) (fibo (- x 2))))) 
)

(defun fibolist (n erglist)
  "Liste von n Fibonaccizahlen: Aufruf mit n und nil"
   (cond ((= 0 n) erglist)
         (T (fibolist (- n 1)
                     (cons (fibo n) erglist))
   )
))
;**********************************************************
;deutlich effektiveres Verfahren:
;jeweils 2 Folgenglieder erzeugen und somit speichern

(defun fiboStep (x2)
    (let ((u (first x2))
          (v (second x2))
         )
         (list v (+ u v))
    )
)

(defun fiboPair (n)
    (cond ((= n 0) (list 0 1))
          (T (fiboStep (fiboPair (- n 1))))
    )
) 
;*********************************************************
;Effektive Variante, fast lazy, ganze Liste aufbauen

(defun fibo-Steps (n LL)
  "Liste der n Fibonaccizahlen, sehr effektive,
   endrekursive Variante
   Aufruf mit n und '(1 1)"
   (if (= 2 n) LL      ;Ende der Rekursion
       (fibo-steps (1- n) (cons (+ (car LL)
                                   (cadr LL)) 
                                LL))))

(defun fibo-nth (n)
  "Berechnet die n-te Fibonaccizahl"
    (car (fibo-steps n '(1 1))))

