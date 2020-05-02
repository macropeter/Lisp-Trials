; Dez-Jän 2013
; WebUntis-csv-Dateien mit Schülerlisten einlesen und
; als xml-Dateien wieder ausgeben

(defstruct schueler
  name vorname klasse zweig faecher)

(setq *my-stream* "schueler.txt")

(defun str2text (txt)  ; Trafo von Symbol in Text mit großem Anfangsbuchstaben
   (string-capitalize (string-downcase (string txt))))


(defun liesn (datei)
  "Von einer Webuntis-csv-Datei die Namen einlesen und als Liste zurückgeben
   den Rest der Zeile jeweils verwerfen"
(with-open-file (my datei :direction :input
                          :if-does-not-exist nil)
    (read-line my) ;Kopfzeile überspringen
    (do ((l1 (read my) (read my nil 'eof)) ;1.Wort lesen, :type Symbol
         (l2 (read my) (read my nil 'eof)) ;2.Wort lesen (nur der 1.Vorname gelesen)
	 (ldummy (read-line my) (read-line my nil 'eof)) ;Rest der Zeile lesen und verwerfen
	 (llist nil (cons (make-schueler :name (str2text l1) :vorname (str2text l2)) llist)))
        ((eq l1 'eof) llist)  ;Abbruchbedingung Lesevorgang am Ende der Datei
        ())))


(defun erweitere (LL kl jg fach &optional fach2)  
    (mapcar (lambda (x) (setf (schueler-zweig x) kl)
		        (setf (schueler-klasse x) jg)
			(setf (schueler-faecher x) (list fach))) ;mehrere Fächer mögl, deshalb Liste
	    LL)
   (reverse LL)) ;zurückgeben (setf gibt sonst nur die letzte Eigenschaft zurück)


(defun schreibs (LL)
  "Ausgabe der Schülerliste im XML-Format
   Eingabe: Liste von SchülerInnen-Records (siehe defstruct)"
(if  (null LL) T
(let ((sinfo (car LL)))   ;1.Datensatz einlesen
    (format t "~%<schuelerIn>~%")
    (format t "<name>~A</name>~%"  (schueler-name sinfo))
    (format t "<vorname>~a</vorname>~%"  (schueler-vorname sinfo))
    (format t "<zweig>~A</zweig>~%" (schueler-zweig sinfo))
    (format t "<jahrgang>~A</jahrgang>~%" (schueler-klasse sinfo))
    (format t "<fach>~a>/fach>~%" (car (schueler-faecher sinfo)))
    (format t "<aktiv>1</aktiv>~%")
    (format t "</schuelerIn>~%") 
    (schreibs (cdr LL)))))  ;Rekursion: Rest der Datensätze

; (schreibs (erweitere (liesn "datei.txt") "A" 8 "Philo"))


