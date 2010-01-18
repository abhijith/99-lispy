;; p1
(defun my-last (lst)
  (cond ((null (rest lst)) lst)
        (t (my-last (rest lst)))))

;; p2
(defun my-but-last (lst)
  (cond ((equal (length lst) 2) lst)
        (t (my-but-last (rest lst)))))

;; p3
(defun element-at (lst pos)
  (cond ((equal 0 pos) (car lst))
        (t (element-at (rest lst) (1- pos)))))
{ a  1 , b 2 }
;; p4
(defun my-length (lst)
  (cond ((equal lst nil) 0)
        (t (+ 1 (my-length (rest lst))))))

;; p5
(defun my-reverse (lst)
  (cond ((null lst) nil)
	(t (append (my-reverse (rest lst)) (list (car lst))))))

;; p6
(defun nthcar (n list)
  (subseq list 0 n))

(defun split-list (lst)
  (let* ((len (length lst))
         (mid (/ len 2)))
    (cond ((oddp len) (values (nthcar (ceiling mid) lst) (nthcdr (floor mid) lst)))
          (t (values (nthcar mid lst) (nthcdr mid lst))))))

(defun palindrome-p (lst)
  (multiple-value-bind (first-half second-half) (split-list lst)
    (equal first-half (reverse second-half))))

;; p7

(defun my-flatten (lst)
  (cond ((null lst) lst)
	((listp (car lst)) (append (my-flatten (car lst)) (my-flatten (cdr lst))))
	(t (append (cons (car lst) nil) (my-flatten (rest lst))))))


(defun my-flatten (lst)
  (let ((first-of-lst (first lst))
	(rest-of-lst (rest lst)))
    (cond ((null lst) lst)
	  ((listp first-of-lst) (append (my-flatten first-of-lst) (my-flatten rest-of-lst)))
	  (t (append (cons first-of-lst nil) (my-flatten rest-of-lst))))))

;; p8 - Eliminate consecutive duplicates of list elements.
;; tail recursive
(defun compress (lst prev acc)
  (cond ((null lst) acc)
	((equal prev (car lst)) (compress (rest lst) (car lst) acc))
	(t (compress (rest lst) (car lst) (append acc (list (car lst)))))))

;;;; hides the implementation specifics. interface to the function is much cleaner unlike the previous function
;; tail recursive
(defun compress (lst)
  (labels ((compress-helper (lst prev acc)
	     (cond ((null lst) acc)
		   ((equal prev (car lst)) (compress-helper (rest lst) (car lst) acc))
		   (t (compress-helper (rest lst) (car lst) (append acc (list (car lst))))))))
    (compress-helper lst nil '())))

;; non tail recursive function

(defun compress (lst)
  (labels ((compress-helper (lst prev)
	     (cond ((null lst) lst)
		   ((equal prev (first lst)) (compress-helper (rest lst) (first lst)))
		   (t (append (list prev) (compress-helper (rest lst) (first lst)))))))
    (compress-helper lst (car lst))))

;;;; p9

(defun clone (char n)
  (cond ((= n 0) nil)
	(t (cons char (clone char (1- n))))))

(defun pack (lst prev count)
  (cond ((null lst) lst)
	((equal prev (car lst))
	 (pack (cdr lst) (car lst) (1+ count)))
	(t (cons (clone prev count)
		 (if (equal (length lst) 1)
		     (list (clone (car lst) 1))
		     (pack lst (car lst) 0))))))

;;;; p10

(defun encode (lst prev count)
  (cond ((null lst) lst)
	((equal prev (car lst))
	 (encode (rest lst) (first lst) (1+ count)))
	(t (cons (list count prev)
		 (if (equal (length lst) 1)
		     (list (list 1 (car lst)))
		     (encode lst (car lst) 0))))))


;;;; p11
(defun encoder (char n)
  (if (> n 1) (list n char) char))

(defun encode2 (lst prev count)
  (cond ((null lst) lst)
	((equal prev (car lst))
	 (encode2 (cdr lst) (car lst) (1+ count)))
	(t (cons (encoder prev count)
		 (if (equal (length lst) 1)
		     (list (encoder (car lst) 1))
		     (encode2 lst (car lst) 0))))))

