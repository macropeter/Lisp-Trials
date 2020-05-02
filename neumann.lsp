; Neumanngenerator (wieviele Bit hat eigentlich Integer bei Lisp?)
; Jänner 2011
; vgl. das baugleiche Haskellprogramm

; ganzzahlige Division
(defun div (x y)
    (truncate (/ x y)))
    
; ganzzahlige Division, manuell gestrickt: ist aber bei großen Zahlen zu langsam!   
(defun divi (x y)
    (do ((z 0 (1+ z))
         (r x (- r y)))
        ((< r y) z)
        ()))
        
; ganzzahlige Division, rekursive Variante: meist Überlauf...     
(defun rdivi-hilf (x y z)
    (if (< x y) z
                (rdivi (- x y) y (1+ z))))
(defun ndivi (x y) (rdivi-hilf x y 0))
               
(defun next-neumann (x)
    (mod (div (* x x) 256) 65536))


; rekursiv: Haskell-style
(defun neumann2 (x n)
   (let ((y (next-neumann x)))
         ;body:
         (if (= n 0) nil
                     (cons y (neumann y (1- n))))))

(defun r-neumann (x n)
     (if (= 0 n) nil
                 (cons x (r-neumann (next-neumann x) (1- n)))))                     

; dotimes: mit setf-Zuweisungen
(defun neumann3 (x n)
    (reverse 
    (let ((erg nil)
          (y x))
         (dotimes (i n erg)
                  (setf erg (cons y erg))
                  (setf y (nextneumann y))
                  ))))

; Lisp-style mit do, dem Schweizermesser der Iteration
(defun neumann (x n)
   (reverse
     (do* ((i 1 (1+ i))     ;Schleifenzähler
           (y x (next-neumann y))    ;Neumannzahl
           (erg (list y) (cons y erg)))   ;Zahlenliste
          ;test    ;Rückgabe
          ((= i n) erg)
          ;body (leer)
          ()))) 
          
; dasselbe mit lokal definierter Funktion      
(defun neumann (x n)
  (reverse
    (labels ((nextneumann (x) (mod (div (* x x) 256) 65536))) 
       (do* ((i 1 (1+ i))     ;Schleifenzähler
             (y x (nextneumann y))    ;Neumannzahl
             (erg (list y) (cons y erg)))   ;Zahlenliste
            ;test    ;Rückgabe
            ((= i n) erg)
            ;body (leer)
            ()))))                      
            
