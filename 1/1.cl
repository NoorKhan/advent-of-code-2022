(defun get-elf-total-calories (input-file)
  (let ((total-calories '())
	(current-calories 0))
    (with-open-file (stream input-file)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(cond ((or (string= #\Return line) (string= "" line))
	       (setf total-calories (append total-calories (list current-calories))
		     current-calories 0))
	      (t (setf current-calories (+ (parse-integer line) current-calories)))))
      (setf total-calories (append total-calories (list current-calories))))
    total-calories))

(setf *total-calories* (get-elf-total-calories "input.txt"))

;; part 1
(defun get-most-calories (total-calories)
  (apply #'max total-calories))

(get-most-calories *total-calories*)

;; part 2
(defun get-top-n-most-calories (total-calories n)
  (let ((rolling-sum 0)
	(current-max 0))
    (dotimes (count n)
      (setf current-max (get-most-calories total-calories)
	    rolling-sum (+ rolling-sum current-max)
	    total-calories (remove current-max total-calories)))
    rolling-sum))

(get-top-n-most-calories *total-calories* 3)


