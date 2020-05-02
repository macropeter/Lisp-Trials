; BubbleSort-Variationen
; Mai 2010

;Innere Schleife: das größte Element wandert ans Ende
(defun bubblehelper (x2)   
  (cond  ((null x2) nil)           ;falls leer, nil zurück
         ((= (length x2) 1) x2)    ;falls nur ein Element:als Liste zurück
	 (T (let ((x (first x2))          ;1.Element
        	  (y (second x2))         ;2.Element
	          (rest (cdr (cdr x2))))  ;Rest der Liste
	         (cond ((> x y) (cons y 
	                              (bubblehelper (cons x rest))))
		       (T       (cons x
	                              (bubblehelper (cons y rest))))
	         )
	     )
	  )
    
  )
)
 
;äußere Schleife (n-1) Durchgänge
(defun bubbleS (n LL) 
    (cond ((= n 0) LL)  ;von bubblehelper organisieren
          (T (bubbleS (- n 1) (bubblehelper LL)))
    )
)

;Starter für BubbleS
(defun bubbleSort (Liste) 
     (bubbleS (- (length Liste) 1) Liste)
)

;---------------------------------------------------
(defun bubble (LL)
   (let ((x (first LL))
         (y (second LL))
         (xy (bubblehelper (list x y)))
         (rest (cdr (cdr LL))))
        (rest)
        
   )
)