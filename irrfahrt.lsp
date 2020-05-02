; Die klassische Irrfahrt
; März 2011

(defparameter *guthaben* 0)

(defun guthaben0 () (setq *guthaben* 0))

(defun einspiel () 
   (if (= (random 2) 0)
            (setq *guthaben* (1+ *guthaben*))
            (setq *guthaben* (1- *guthaben*))))
 
(defun irrfahrt (n)  ;fast deklarative Variante
   (progn (guthaben0)
          (dotimes (i n 'done)
                   (progn (einspiel)
                          (format t "~A ~%" *guthaben*))
                          ))) 

(defun irrfahrt2 (n)  ;eher funktional: aber mit globVar
    (do ((i 1 (1+ i))
         (erg nil (cons *guthaben* erg)))
        ((> i n) (reverse erg))  ;Rückgabe: umgedrehte Liste
        (format t "~A ~%" (einspiel))))

(defun f-einspiel nil   ; f(unktionales)-einspiel
    (if (= (random 2) 0) 1 -1))

(defun irrfahrt3 (n)   ;arbeitet nur mit lokalen Vars
    (do ((i        1   (1+ i))
         (guthaben 0   (+ guthaben (f-einspiel)))
         (erg      nil (cons guthaben erg)))
        ((> i n) (reverse erg))  ; gibt am Ende das Ergebnis als Liste
        (format t "~A ~%" guthaben))) ;body: Ausgabe


