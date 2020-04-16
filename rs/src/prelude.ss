(define (open-input-file fname)
  (let ((fd (rt-open-read fname)))
    (vector 'port fname fd)))

(define (open-output-file fname)
  (let ((fd (rt-open-write fname)))
    (vector 'port fname fd)))

(define (current-input-port)
  (let ((fd (rt-current-input-port)))
    (vector 'port "stdin" fd)))

(define (current-output-port)
  (let ((fd (rt-current-output-port)))
    (vector 'port "stdout" fd)))

(define (current-error-port)
  (let ((fd (rt-current-error-port)))
    (vector 'port "stdout" fd)))
