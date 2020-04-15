(define (open-input-file fname)
  (let ((fd (rt-open-read fname)))
    (vector 'port fname fd)))

(define (open-output-file fname)
  (let ((fd (rt-open-write fname)))
    (vector 'port fname fd)))

(define (current-input-port)
  (let ((fd (rt-current-input-port)))
    (vector 'port "stdin" fd)))
