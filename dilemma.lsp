; Juli 2011
; Eigenes Konzept nach "Land of Lisp" Kapitel 9
; Neuaufnahme: Jänner 2013

(defvar *matrix* (list (list (cons 1 1) (cons 0 5))
                       (list (cons 5 0) (cons 3 3))))

(defparameter *erg-liste* nil) ;history zum Auswerten für die Teilnehmer
                               ;das letzte Erebnis wird vorne angehängt

(defun clear-ergebnisse () (setq *erg-liste* nil))

;das allg Spielerobjekt
(defstruct spieler (history nil) ;
	           (gewinn 0)) ; letzte Gewinnüberweisung
; der gemeine Zufallsspieler
(defmethod setze-spieler (s)
  "::Spieler->Bool; Handlung des Spielers: betrügen (nil) oder kooperieren (T)"
    (if (zerop (random 2))
        nil
        T))
(defmethod rueckmeld-spieler (s gew)
  (setf (spieler-gewinn s) gew))
        
(defun spiel1 (sp1 sp2)
 "::spieler->spieler->(gewinn . gewinn); zwei Spieler setzen gegeneinander"
(let ((x (setze-spieler sp1))
      (y (setze-spieler sp2)))
  (let ((erg (cond ((and x y) (cons 3 3))
                   ((and (not x) y) (cons 5 0))
                   ((and x (not y)) (cons 0 5))
                   (T (cons 1 1)))))
          (push erg *erg-liste*)  ;Seiteneffekt: history updaten
       	  (rueckmeld-spieler sp1 (car erg)) ;Seiteneffekt: Rückmeldung an Spieler
	  (rueckmeld-spieler sp2 (cdr erg))
          erg)))
 

(defstruct (nihilist (:include spieler)))
(defmethod setze-spieler ((s nihilist)) nil) ; betrügt immer

(defstruct (alternator (:include spieler))
	   zuletzt) ;Spieler mit Gedächtnis für die letzte Runde
                    ;spielt alternierend
(defmethod setze-spieler ((s alternator))
  (not (alternator-zuletzt s)))

(defun auswertung (ergliste)
  "::(ergebnisse)->[sum1 sum2]: Berechnen der Summen"
   (let ((sum1 (apply #'+ (mapcar #'car ergliste)))
         (sum2 (apply #'+ (mapcar #'cdr ergliste))))
;Seiteneffekt: Ergebnis ausgeben
    (princ "Summe Spieler 1 = ")
    (princ sum1)
    (terpri)
    (princ "Summe Spieler 2 = ")
    (princ sum2)
    (terpri)
    (princ "Gesamtergebnis = ")
    (princ (+ sum1 sum2))
;Ergebnis zurückgeben
    (cons sum1 sum2)))  

(defun duell (a b n)
  (loop for i below n collect (spiel1 a b)))
  
;(auswertung (duell a b 10))

;------------------ Baustelle --------------------------- 

(defstruct (tit4tatter (:include spieler))
	   zuletzt)
(defmethod setze-spieler ((s tit4tatter))
; (let ((zuz (tit4tatter-zuletzt s)) 
  (if (tit4tatter-zuletzt s) 
      (setf (tit4tatter-zuletzt s) nil)
      (setf (tit4tatter-zuletzt s) T)))

;------------------ Archiv ------------------------------

(defun spiel (x y)
  "::Bool->Bool->(gewinn . gewinn); zwei Spieler setzen gegeneinander"
  (cond ((and x y) (push (cons 3 3) *erg-liste*))
        ((and (not x) y) (push (cons 5 1) *erg-liste*))
        ((and x (not y)) (push (cons 1 5) *erg-liste*))
        (T (push (cons 1 1) *erg-liste*))))
