;InsertionSort
;vgl die Haskellvariante
;Mai 2010

(defun sorthelper (glist x)  ;x in eine geordnete Liste einfügen
    (cond ((null  glist)(list x))
          (T (let ((k (car glist)))
                  (cond ((< x k)(cons x glist))
                        (T      (cons k
                                      (sorthelper (cdr glist) x))))
             )
          )
    )
)

(defun sorthelper2 (liste hliste)  ;Elems von einer Liste (liste)
    (cond ((null liste) hliste)    ;in eine andere (hliste) einordnen
          (T            (let ((neuliste (sorthelper hliste (car liste))))
                             (sorthelper2 (cdr liste) neuliste)))
    )
)

(defun insort (LL)
    (sorthelper2 LL nil)   ;die geordnete Liste ist zunächst leer
)