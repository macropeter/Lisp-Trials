; BogoSort (oder StupidSort) nach www.blödsort.de
; März 2011
; Oktober 2011

(defun istSortiert (LL)
  "Prüft nach, ob eine Liste sortiert ist"
  (cond ((null (cdr LL)) T)    ;nur noch ein Element in der Liste
        (T (and (< (car LL) 
                   (cadr LL)) 
                (istSortiert (cdr LL))))))

(defun ListeOhneN (n LL)
   (if (zerop n) (cdr LL)
                 (cons (car LL) 
                       (ListeOhneN (1- n) (cdr LL)))))

(defun holeNthHeraus (n LL)
  "Liste aus dem nth und der restlichen Liste: mit car
   erhält man das Element, mit second die Liste ohne das Element"
   (list (nth n LL)
         (ListeOhneN n LL)))

(defun list1bisN (n)
  "Liste von 1..n"
   (do ((i n (1- i))
        (erg nil (cons i erg)))
       ((= i 0) erg)
       nil))
       
(defun permutation (LL)
 "Eine zufällige Permutation einer eingegebenen Liste erzeugen"
(labels ((permutationHelp (LL OutLL)
   (if (null LL) OutLL
         (let ((redux (holeNthHeraus (random (length LL)) LL)))
              (permutationHelp (second redux)  
                               (cons (car redux) OutLL)))))) 
    (permutationHelp LL nil)))
    


(defun bogosort (LL)
  "Sortieren durch Erzeugen zufälliger Permutationen solange bis eine Sortierung entstanden ist; Achtung: terminiert unter Umständen gar nicht und hat eine entsetzliche Performance!"
   (if (istSortiert LL) LL
       (bogosort (permutation LL))))
 
; Oktober 2011
       
(defun liste1n (n)  ;leider verkehrt herum
   (unless (zerop n) (cons n (liste1n (1- n)))))
        
(defun liste1nn (x) ;richtige Reihenfolge
   "Liste von 1..x, ohne do aber mit lokaler Fkt"
   (labels ((helper (n) (unless (zerop n) (cons n (liste1n (1- n))))))
       (reverse (helper x))))

