;;; pretty print json exactly the way the json's JSON.stringify(obj, null, 2) does it!
;;; it's probably *not* complete and probably quite buggy!
;;;
;;; csi -R ppjson -e '(ppjson `#(((hello . 13))))'
;;; [
;;;   {
;;;     "hello": 13
;;;   }
;;; ]

(module ppjson (ppjson)

(import chicken scheme)
(use data-structures srfi-1 ports)

;; sol = start of line (if sol == true we must indent)
(define (ppjson obj #!optional (depth 0) (sol #f))

  (define (out str #!optional (depth depth))
    (if sol
        (display (make-string depth #\space)))
    (display str))
  
  (define (ostr s)
    (with-output-to-string (lambda () (write s))))

  (define (nl)
    (newline)
    (set! sol #t))
  
  (cond ((vector? obj)
         (out "[")
         (nl)
         (for-each (lambda (i) (ppjson i (+ depth 2) #t)
                      (display ",")
                      (newline))
                   (drop-right (vector->list obj) 1))
         (ppjson (last (vector->list obj)) (+ depth 2) #t)
         (nl)
         (out "]"))
        ((string? obj)
         (out (ostr obj)))
        ((list? obj)
         (map (lambda (pair)
                (out "{") (nl)
                (out (conc (ostr (symbol->string (car pair))) ": ") (+ depth 2))
                (ppjson (cdr pair) (+ depth 2))
                (nl)
                (out "}"))
              obj))
        ((number? obj)
         (out obj))
        ((eq? obj #f) (out "false"))
        ((eq? obj #t) (out "true"))
        ((eq? obj 'null) (out "null")))
  (void))

)
