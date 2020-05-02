(defun bsp (x)
       (* 2 x))

(defun nest (f x n)
       (cond ((eq n 0) (funcall f x))
	     (T (nest f (funcall f x) (1- n)))))

(defun nestlist (f x n)
       (cond ((eq n 0) (funcall f x))
	     (T (nest f (funcall f x) (1- n)))))