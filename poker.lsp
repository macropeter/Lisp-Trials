; Pokertest für Zufallszahlen (0,1-Liste)
; Dez 2010

(defun hpoker (x liste nr)
     (if (null liste)
            nr
            (if (= x (car liste))
                   (hpoker (car liste) (cdr liste) nr)
                   (hpoker (car liste) (cdr liste) (+ nr 1)))))

(defun pokertest (liste)
    (if (not (null liste)) (hpoker (car liste) (cdr liste) 0)))

; 01-Liste der Länge nr erstellen
(defun liste01 (nr)
(let (ll) (dotimes (i nr) 
                   (setf ll (cons (random 2) ll))) 
          ll))

; n-mal 01Listen von der Länge 50 erstellen und auswerten
(defun runtest (n) 
   "n-mal 01Listen von der Länge 50 erstellen und auswerten"
   (let (erglist) (dotimes (i n)
                           (setf erglist (cons (pokertest (liste01 50)) erglist)))
                  (format t "Anzahl der Läufe:")         
                  erglist))
