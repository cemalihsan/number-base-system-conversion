#lang racket

;I put example execution conversion of each function and how they took input. For Binary Decimal and Octal conversion, directly write numbers.
;For Hexa conversion you need to write input as a string like "FF"


(define (hex-digits number)
    (cond ((< number 10) number)
          ((equal? number 10) 'A)
          ((equal? number 11) 'B)
          ((equal? number 12) 'C)
          ((equal? number 13) 'D)
          ((equal? number 14) 'E)
          ((equal? number 15) 'F)
          (else (error "Not in hex range"))))

(define (bin-digits hexVal)
  (cond
    ((equal? hexVal "0") 0)
    ((equal? hexVal "1") 1)
    ((equal? hexVal "2") 10)
    ((equal? hexVal "3") 11)
    ((equal? hexVal "4") 100)
    ((equal? hexVal "5") 101)
    ((equal? hexVal "6") 110)
    ((equal? hexVal "7") 111)
    ((equal? hexVal "8") 1000)
    ((equal? hexVal "9") 1001)
    ((equal? hexVal "A") 1010)
    ((equal? hexVal "B") 1011)
    ((equal? hexVal "C") 1100)
    ((equal? hexVal "D") 1101)
    ((equal? hexVal "E") 1110)
    ((equal? hexVal "F") 1111)
    ((equal? hexVal "a") 1010)
    ((equal? hexVal "b") 1011)
    ((equal? hexVal "c") 1100)
    ((equal? hexVal "d") 1101)
    ((equal? hexVal "e") 1110)
    ((equal? hexVal "f") 1111)))

(define get-Type
  (lambda (x)
    (cond ((number? x) "Number")
          ((pair? x) "Pair")
          ((string? x) "String")
          ((list? x) "List"))))

(define (menu)
    (display "From which Base\n1. Decimal\n2. Binary\n3. Octal\n4. Hexadecimal\n")
    (define from-base(read))
    (newline)
    (display "To which Base\n1. Decimal\n2. Binary\n3. Octal\n4. Hexadecimal\n")
    (define to-base(read))
    (newline)
    (display "For Hexadecimal please write as string(in double qotetion.)")
    (newline)
    (newline)
    (display "Enter number to convert:" )
    (define value(read))
    (newline)
    (cond ((and(eq? 1 from-base) (eq? 1 to-base)) (display value))
          ((and(eq? 1 from-base) (eq? 2 to-base)) (dec-bin value))
          ((and(eq? 1 from-base) (eq? 3 to-base)) (dec-oct value))
          ((and(eq? 1 from-base) (eq? 4 to-base)) (dec-hex value))
          ((and(eq? 2 from-base) (eq? 1 to-base)) (bin-dec value))
          ((and(eq? 2 from-base) (eq? 2 to-base)) (display value))
          ((and(eq? 2 from-base) (eq? 3 to-base)) (bin-oct value))
          ((and(eq? 2 from-base) (eq? 4 to-base)) (bin-hex value))
          ((and(eq? 3 from-base) (eq? 1 to-base)) (oct-dec value))
          ((and(eq? 3 from-base) (eq? 2 to-base)) (oct-bin value))
          ((and(eq? 3 from-base) (eq? 3 to-base)) (display value))
          ((and(eq? 3 from-base) (eq? 4 to-base)) (oct-hex value))
          ((and(eq? 4 from-base) (eq? 1 to-base)) (hex-dec value))
          ((and(eq? 4 from-base) (eq? 2 to-base)) (hex-bin value))
          ((and(eq? 4 from-base) (eq? 3 to-base)) (hex-oct value))
          ((and(eq? 4 from-base) (eq? 4 to-base)) (display value))
          (else "Wrong selection. Please select between 1 2 3 and 4")))
        

(define (bin-dec n)
  (if (zero? n)
      n
      (+ (modulo n 10) (* 2(bin-dec(quotient n 10))))))

(define (bin-oct n)
  (let ((decimal (bin-dec n)))
  (if (zero? decimal)
      decimal
      (dec-oct decimal))))

(define (bin-hex number)
  (let ((hexVal (bin-dec number)))
    (if (zero? hexVal)
        0
        (dec-hex hexVal))))


(define (dec-bin n)
  (if (< n 1)
      0
      (+(remainder n 2)(* 10 (dec-bin (quotient n 2))))))

(define (dec-oct n)
  (if (zero? n)
      0
      (+(modulo n 8) (* 10(dec-oct(quotient n 8))))))


(define (dec-hex n)
    (if (not (list? n))
        (dec-hex (cons n '()))
        (if (eq? (car n) 0)
            (cdr n)
            (dec-hex (cons (quotient (car n) 16) (cons (hex-digits (modulo (car n) 16)) (cdr n)))))))


(define (oct-dec n)
  (if (zero? n)
      0
      (+(modulo n 10)(* 8 (oct-dec (quotient n 10))))))
      
(define (oct-bin n)
  (let ((decimal(oct-dec n)))
  (if (< decimal 1)
      0
      (dec-bin decimal))))

(define (oct-hex number)
  (let ((hexVal(oct-dec number)))
    (if (zero? hexVal)
        0
        (dec-hex hexVal))))


(define (hex-bin hexValue)
  (binaryValueDigit (- (string-length hexValue) 1) (string-length hexValue) 0 hexValue))

(define (binaryValueDigit index pos digits hexNum)
  (if (< index 0)
      0
      (+ (* (bin-digits (substring hexNum index pos)) (expt 10 digits)) (binaryValueDigit (- index 1) (- pos 1) (+ digits 4) hexNum))))

(define (hex-dec hexValue)
  (let ((binary(hex-bin hexValue)))
    (if (zero? binary)
        0
        (bin-dec binary))))

(define (hex-oct hexValue)
  (let ((octal(hex-bin hexValue)))
    (if (zero? octal)
        0
        (bin-oct octal))))

(write 'decimal-binary: ) (dec-bin 26)
(write 'decimal-octal: ) (dec-oct 26)
(write 'decimal-hexa: ) (dec-hex 26)
(write 'binary-decimal: ) (bin-dec 11010)
(write 'binary-octal: ) (bin-oct 11010)
(write 'binary-hexa: ) (bin-hex 11010)
(write 'octal-decimal: ) (oct-dec 32)
(write 'octal-binary: ) (oct-bin 32)
(write 'octal-hexa: ) (oct-hex 32)
(write 'hexa-decimal: ) (hex-dec "1A")
(write 'hexa-binary: ) (hex-bin "1A")
(write 'hexa-octal: ) (hex-oct "1A")


(menu)


