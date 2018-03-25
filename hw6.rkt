#lang racket

(provide hours
         entry entry? entry-key entry-value
         ram-read ram-write diff-rams
         extract bits->int int->bits int->bits-width
         conf conf? conf-cpu conf-ram
         diff-configs incr-pc do-load do-store
         do-add do-sub
         do-input do-output
         do-jump do-skipzero do-skippos do-skiperr
         do-loadi do-storei
         next-config
         init-config symbol-table assemble
         simulate remainder-prog
         reverse-prog)

;************************************************************
; CS 201a HW #6  DUE Thursday, November 9th at 11:59 pm, 
; via the submit system on the Zoo.
;************************************************************
; Name: Olivia Roth
; Email address: olivia.roth@yale.edu
;************************************************************

; Computer science topics: TC-201 assembler and simulator,
; assembly language programs for remainder and reverse.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 19)

;************************************************************

; A table is a list of entries, where each entry has two fields: key and value.
; The constructor for entries is entry, the type predicate is entry?, and the
; two selectors are entry-key and entry-value.

(struct entry (key value) #:transparent)

; Random access memory (RAM)

; We represent the contents of a memory register as
; a list of 16 bits, each either 0 or 1.
; The contents of the RAM is represented as a list giving
; the contents of memory register 0, memory register 1,
; and so on, up to some address n, where n is at most 4095.
; Those memory registers whose contents are not explicitly
; listed are assumed to contain 16 zeroes.

; Examples of RAMs.

(define ram-ex1
  '((0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1)
    (0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 0 1 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)))
(define ram-ex-neg
  '((1 0 0 0  0 0 1 0  0 0 1 0  1 0 1 0)
    (0 0 0 0  0 0 1 0  0 0 1 0  1 0 1 0)
    (1 0 0 0  0 0 0 0  0 0 0 0  1 1 1 1)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 1 1 1)
    (1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1)
    (0 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1)))
(define ram-ex2
  '((0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1)
    (0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 1)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 1 0  0 1 0 0)))

;************************************************************
; ** problem 1 ** (9 points)
; Write three procedures

; (ram-read address ram)
; (ram-write address contents ram)
; (diff-rams ram1 ram2)

; (ram-read address ram)
; takes a memory address and a ram
; and returns a list of 16 bits giving the contents
; of the memory register in ram with the given address.

; (diff-rams ram1 ram2)
; takes two RAMs and returns a list indicating the memory registers 
; which have different contents in the two RAMs.
; The format of the list is a list of triples giving
; a memory address, the contents of that memory register
; in ram1, and the contents of that memory register
; in ram2.  The addresses should be in increasing order.

; (ram-write address contents ram)
; takes a memory address (address), a list of 16 bits (contents) and a ram,
; and returns a ram representing the result of copying the contents 
; into the memory register of ram specified by the memory address.

; Examples

;> (ram-read 0 ram-ex1)
;'(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)

;> (ram-read 6 ram-ex2)
;'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

;> (diff-rams ram-ex1 ram-ex2)
;'((1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;  (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0)))

;> (diff-rams '() '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
;'()

;> (diff-rams ram-ex1 (ram-write 2 '(0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1) ram-ex1))
;'((2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1)))

;> (diff-rams ram-ex2 (ram-write 5 '(1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0) ram-ex2))
;'((5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0)))

;> (diff-rams ram-ex1 (ram-write 1 '(0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1) ram-ex1))
;'((1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1)))

;************************************************************

(define (ram-read address ram)
  (cond ((empty? ram)'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
        ((= address 0)(car ram))
        (else (ram-read (- address 1) (cdr ram)))))

(define (ram-write address contents ram)
  (ram-write-HELP address contents ram '()))

;does what ram-write is supposed to, but uses tail recursion, so new variable, "new" is added
(define (ram-write-HELP address contents ram new)
  (cond ((= address 0)
         (if (remaining-empty? ram)
             (append new (append (cons contents '())))
             (append new (append (cons contents (cdr ram))))))
        ((empty? ram) (ram-write-HELP (- address 1) contents ram (append new (cons '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '()))))
        (else (ram-write-HELP (- address 1) contents (cdr ram) (append new (cons (car ram) '()))))))
    
(define (diff-rams ram1 ram2)
  (diff-rams-HELP 0 ram1 ram2 '()))

;does what diff-rams is supposed to, but uses tail recursion, so new variable, "final" is added
(define (diff-rams-HELP num ram1 ram2 final)
  (cond ((or (and (empty? ram1) (empty? ram2)) (and (remaining-empty? ram1)(remaining-empty? ram2)))
                      final)
        ((and (empty? ram1) (equal? (car ram2) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
                     (diff-rams-HELP (+ 1 num) ram1 (cdr ram2) final))
        ((and (empty? ram2) (equal? (car ram1) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
                     (diff-rams-HELP (+ 1 num) (cdr ram1) ram2 final))
        ((or (empty? ram1) (equal? ram1 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
                     (diff-rams-HELP (+ 1 num) ram1 (cdr ram2) (append final (list (cons num (append (cons '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '()) (cons (car ram2) '())))))))
        ((or (empty? ram2)(equal? ram2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
                     (diff-rams-HELP (+ 1 num) (cdr ram1) ram2 (append final (list (cons num (append  (cons (car ram1) '()) (cons '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '())))))))
        ((not (equal? (car ram1) (car ram2)))
                     (diff-rams-HELP (+ 1 num) (cdr ram1) (cdr ram2) (append final (list (cons num (append (cons (car ram1) '()) (cons (car ram2) '())))))))
        (else ;
              (diff-rams-HELP (+ 1 num) (cdr ram1) (cdr ram2) final))))

;checks if the ram is empty/only has rows of 0000000's left
(define (remaining-empty? ram1)
  (cond ((empty? ram1) #t)
        ((equal? (cdr (car ram1)) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) (remaining-empty? (cdr ram1)))
        (else #f)))

;************************************************************
; ** problem 2 ** (10 points)
; Write four procedures:

; (extract i j lst)
; (bits->int lst) 
; (int->bits n)
; (int->bits-width n w)

; (extract i j lst) 
; takes nonnegative integers i and j and a list lst
; and returns the list of elements of lst indexed i through j.
; You may assume i and j are at least 0 and less than the
; length of the list, and i is less than or equal to j.
; As in list-ref, list elements are indexed starting with 0.

; (bits->int lst) takes a list of bits lst
; and returns the value of the nonnegative number 
; represented in binary by those digits.

; (int->bits n) takes a nonnegative integer n
; and returns the list of bits representing n in 
; unsigned binary.
; Note that for 0 the answer is (0) but for
; all other numbers the answer starts with 1.

; (int->bits-width n w) takes a nonnegative integer n
; and returns a list of w bits representing n in 
; unsigned binary.
; If n cannot be correctly represented in binary using
; w bits, the string "field too small" should be returned.

; Examples

;> (extract 1 3 '(a b c d e))
;'(b c d)

;> (extract 4 4 '(a b c d e))
;'(e)

;> (bits->int '(0))
;0

;> (bits->int '(0 0 0 1 1 0))
;6

;> (int->bits 0)
;'(0)

;> (int->bits 6)
;'(1 1 0)

;> (int->bits-width 14 8)
;'(0 0 0 0 1 1 1 0)

;> (int->bits-width 14 3)
;"field too small"

;************************************************************

(define (extract i j lst)
  (cond ((= j 0) (cons (car lst) '()))
        ((> i 0)(extract (- i 1) (- j 1) (cdr lst)))
        (else (cons (car lst) (extract i (- j 1) (cdr lst))))))

(define (bits->int lst)
  (bin2decASSIST lst (- (length lst) 1) 0));passes the binary number, the number of items in the list minus one
;(to give you the eventual exponent you need to calculate the decimal number), and the deimal number

(define (bin2decASSIST binList place sum);takes the list of binary numbers, an integer denoting the exponent, and the decimal sum. It goes through binList, adding their decimal value to sum
  (if (equal? (length binList) 0);have we gone through all the given digits and is the binary list empty?
      sum;then return the sum you've accumulated
      (if (equal? (car binList) 1);if the first item in the list is a one, 
          (bin2decASSIST (cdr binList) (- place 1) (+ sum (expt 2 place)));pass the rest of the list through the
          ;recursive method again. Decrease 'place' (which is the number that denotes the expoential value of that
          ;item in the list) by one, increase the sum by 2 to the power of 'place'
          (bin2decASSIST (cdr binList) (- place 1) sum))));pass the rest of the list through the recursive method
  ;again. Decrease 'place' (which is the number that denotes the expoential value of that item in the list) by one


(define (int->bits n)
  (dec2binASSIST n '()))

(define (dec2binASSIST decNum binList)
;this method takes the decimal number and a list that records each binary number calculated
;if the decimal number is 0, end the recursive loop
  (if (equal? decNum 0)
      (if (empty? binList)
          '(0)
          binList)
      (dec2binASSIST (quotient decNum 2) (append (list(remainder decNum 2)) binList))))
;otherwise, pass the quotient of the decimal number
;and binList with with the remainder of decNum and 2 appended to the front
  

(define (int->bits-width n w)
  (let ([binNum (int->bits n)])
    (if (> (length binNum) w)
        "field too small"
        (add-Zeros binNum (- w (length binNum))))))

(define (add-Zeros binNum w)
  ;returns binNum if w is 0
  ;Otherwise cons a 0 onto the front of binNum, and call add-Zeros again decreasing w by 1
  (cond ((= 0 w) binNum)
        (else (add-Zeros (cons 0 binNum) (- w 1)))))


;************************************************************
; Next we develop a simulator for the TC-201

; For the TC-201 Central Processing Unit (CPU), the contents of the registers 
; are represented by a table with entries giving the contents of the CPU 
; registers ** in this order **.

; the accumulator (acc)
; the program counter (pc)
; the run flag (rf)
; the arithmetic error bit (aeb)

; Each entry is a list containing 
; a symbol (one of 'acc, 'pc, 'rf, 'aeb)
; a list of bits of the correct length,
; namely, 16 bits for the acc, 12 bit for
; the pc, and 1 bit each for rf and aeb.

; Examples

(define cpu-ex1 
  (list
   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
   (entry 'pc '(0 0 0 0 0 0 0 0 0 1 1 1))
   (entry 'rf '(1))
   (entry 'aeb '(0))))

(define cpu-ex3
  (list
   (entry 'acc '(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
   (entry 'pc '(0 0 0 0 0 0 0 0 0 1 1 1))
   (entry 'rf '(1))
   (entry 'aeb '(0))))

(define cpu-ex2 
  (list
   (entry 'acc '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
   (entry 'pc '(0 0 0 0 0 0 0 0 0 1 1 1))
   (entry 'rf '(1))
   (entry 'aeb '(1))))

; A configuration of the TC-201 is a struct with two fields:
; (1) the contents of the CPU registers, in the above format,
; (2) the contents of the RAM, in the format of problem 1.

(struct conf (cpu ram) #:transparent)

; Note that the constructor is conf, the type-predicate
; is conf?, and the selectors are conf-cpu, conf-ram.

; Examples

(define config-ex1 (conf cpu-ex1 ram-ex1))
(define config-ex-pos (conf cpu-ex1 ram-ex-neg))
(define config-ex-neg (conf cpu-ex3 ram-ex-neg))
(define config-ex2 (conf cpu-ex2 ram-ex2))

;************************************************************
; ** problem 3 ** (10 points)
; Write four procedures

; (diff-configs config1 config2)
; (incr-pc n config)
; (do-load address config)
; (do-store address config)

; (diff-configs config1 config2)
; takes two configurations and returns a list showing where they differ, 
; as a list of triples, giving the name (or address) of the
; register, the contents in config1 and the contents in config2.  
; The order should be CPU registers first (in order: acc, pc, rf, aeb) 
; and then memory registers in increasing order of addresses.

; (incr-pc n config)
; takes a nonnegative integer n and a TC-201 configuration config
; and returns the TC-201 configuration that is obtained by adding n 
; to the value of pc.  Note that the sum should be taken modulo 4096.  
; (Racket has a modulo procedure.)

; (do-load address config)
; takes a memory address and a TC-201 configuration, and returns the TC-201 
; configuration that is obtained by copying the contents
; of the given memory address into the accumulator.
; The values of all other registers (including the pc) are unchanged.

; (do-store address config)
; takes a memory address and a TC-201 configuration, and returns the TC-201 
; configuration that is obtained by copying the contents of the accumulator 
; into the given memory address.
; The values of other all registers (including the pc) are unchanged.

; Examples

;> (diff-configs config-ex1 config-ex2)
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;  (aeb (0) (1))
;  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;  (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0)))

; The first result is shown in full -- you may produce an equivalent
; configuration.  Subsequent results are shown using diff-configs.

;> (incr-pc 1 config-ex1)
;(conf
; (list
;  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;  (entry 'pc '(0 0 0 0 0 0 0 0 1 0 0 0))
;  (entry 'rf '(1))
;  (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;> (diff-configs config-ex2 (incr-pc 4090 config-ex2))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1)))

;> (diff-configs config-ex1 (do-load 1 config-ex1))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)))

;> (diff-configs config-ex2 (do-load 12 config-ex2))
;'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

;> (diff-configs config-ex1 (do-store 5 config-ex1))
;'((5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)))

;>  (diff-configs config-ex2 (do-store 0 config-ex2))
;'((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)))

;************************************************************

(define (diff-configs config1 config2)
  (append
   (diff-HELP (entry-key (car (conf-cpu config1))) (entry-value (car (conf-cpu config1))) (entry-value (car (conf-cpu config2))))
   (diff-HELP (entry-key (cadr (conf-cpu config1))) (entry-value (cadr (conf-cpu config1))) (entry-value (cadr (conf-cpu config2))))
   (diff-HELP (entry-key (caddr (conf-cpu config1))) (entry-value (caddr (conf-cpu config1))) (entry-value (caddr (conf-cpu config2))))
   (diff-HELP (entry-key (cadddr (conf-cpu config1))) (entry-value (cadddr (conf-cpu config1))) (entry-value (cadddr (conf-cpu config2))))
   (diff-rams (conf-ram config1) (conf-ram config2))))

;checks if the values from config 1 and 2 are different
;if they are, cons variable dig, which is the key, onto the two different results
;otherwise, return an empty list
(define (diff-HELP dig item1 item2)
  (if (equal? item1 item2)
      '()
      (list (cons dig (append (cons item1 '()) (cons item2 '()))))))

(define (incr-pc n config)
  (let ([dec (+ (bits->int (entry-value (cadr (conf-cpu config)))) n)])
    (if (>= dec 4096)
        (conf (list
               (entry 'acc (entry-value (car (conf-cpu config))))
               (entry 'pc (int->bits-width (modulo dec 4096) 12))
               (entry 'rf (entry-value (caddr (conf-cpu config))))
               (entry 'aeb (entry-value (cadddr (conf-cpu config)))))
               (conf-ram config))
        (conf (list
               (entry 'acc (entry-value (car (conf-cpu config))))
               (entry 'pc (int->bits-width dec 12))
               (entry 'rf (entry-value (caddr (conf-cpu config))))
               (entry 'aeb (entry-value (cadddr (conf-cpu config)))))
               (conf-ram config)))))

(define (do-load address config)
  ;(display "do-load\n")
  (conf (list
               (entry 'acc (ram-read address (conf-ram config)))
               (entry 'pc (entry-value (cadr (conf-cpu config))))
               (entry 'rf (entry-value (caddr (conf-cpu config))))
               (entry 'aeb (entry-value (cadddr (conf-cpu config)))))
               (conf-ram config)))

(define (do-store address config)
  ;(display "do-store\n")
  (conf (conf-cpu config)
        (ram-write address (entry-value (car (conf-cpu config))) (conf-ram config))))
	   
;************************************************************
; ** problem 4 ** (10 points)
; Write two procedures

; (do-add address config)
; (do-sub address config)

; (do-add address config)
; takes a memory address and a TC-201 configuration
; and returns a TC-201 configuration in which
; the contents of the memory register addressed has
; been added to the contents of the accumulator.

; (do-sub address config) is similar, except that the
; contents of the memory register addressed has
; been subtracted from the contents of the accumulator.

; Note that if the result is zero, the answer should
; be +0, not -0.
 
; If the result can be correctly represented in 16-bit sign/magnitude
; then the arithmetic error bit (aeb) should also be set to 0.

; If the result cannot be correctly represented in 16-bit sign/magnitude
; then the arithmetic error bit (aeb) should also be set to 1.
; In this case, the result in the accumulator should be 
; 16 zeroes, representing +0.

; The contents of registers other than the accumulator and the
; arithmetic error bit should be unchanged.

; Examples

;> (diff-configs config-ex1 (do-add 3 config-ex1))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1)))

;> (diff-configs config-ex2 (do-add 3 config-ex2))
;'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1))
;  (aeb (1) (0)))

;> (diff-configs config-ex1 (do-sub 3 config-ex1))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1)))

;> (diff-configs config-ex2 (do-sub 3 config-ex2))
;'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (1 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1))
;  (aeb (1) (0)))

;>  (let ((config (conf cpu-ex1 '((0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))) (diff-configs config (do-add 0 config)))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (aeb (0) (1)))

;************************************************************

(define (do-add address config)
  ;(display "do-add\n")
  (let ([accNum (bits->int (cdr (entry-value (car (conf-cpu config)))))]
        [memNum (bits->int (cdr (ram-read address (conf-ram config))))]
        [memMag (car (ram-read address (conf-ram config)))]
        [accMag (car (entry-value (car (conf-cpu config))))])
    (cond ((and (= accMag 1) (= memMag 1))
           ;(display 1)
           (conf (list
               (entry 'acc (negZer (cons 1 (checkACC (int->bits-width (+ accNum memNum) 15)))))
               (entry 'pc (entry-value (cadr (conf-cpu config))))
               (entry 'rf (entry-value (caddr (conf-cpu config))))
               (entry 'aeb (isAEB (+ accNum memNum))))
               (conf-ram config)))
          ((= accMag 1);neg acc
           ;(display 2)
           (conf (list
               (entry 'acc (negZer (cons (isPos accNum memNum) (checkACC(int->bits-width (abs (+ (- 0 accNum) memNum)) 15)))))
               (entry 'pc (entry-value (cadr (conf-cpu config))))
               (entry 'rf (entry-value (caddr (conf-cpu config))))
               (entry 'aeb (isAEB (abs (+ (- 0 accNum) memNum)))))
               (conf-ram config)))
          ((= memMag 1);neg mem
           ;(display 3)
           (conf (list
               (entry 'acc (negZer (cons (isPos memNum accNum) (checkACC (int->bits-width (abs (+ accNum (- 0 memNum))) 15)))))
               (entry 'pc (entry-value (cadr (conf-cpu config))))
               (entry 'rf (entry-value (caddr (conf-cpu config))))
               (entry 'aeb (isAEB (abs (+ accNum (- 0 memNum))))))
               (conf-ram config)))
          (else
           ;(display 4)
           (conf (list
               (entry 'acc (negZer (cons 0 (checkACC(int->bits-width (+ accNum memNum) 15)))))
               (entry 'pc (entry-value (cadr (conf-cpu config))))
               (entry 'rf (entry-value (caddr (conf-cpu config))))
               (entry 'aeb (isAEB (+ accNum memNum))))
               (conf-ram config))))))

;checks whether the number is greater than aeb
(define (isAEB num)
  (if (> num 32767)
      '(1)
      '(0)))
;if the number is too large to be represented in <22^15, return 15 0's
(define (checkACC num)
  (if (equal? num "field too small")
      '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0)
      num))
;if the first element in the list is 1 and the rest of the list is 0, make the list +0
;otherwise, return the list
(define (negZer lst)
  (if (and (equal? (car lst) 1) (equal? 0 (bits->int (cdr lst))))
      '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
      lst))
;checks whether the numbers are equal or if one is larger than the other
(define (isPos n1 n2)
  (if (= n1 n2)
      0
      (if (> n1 n2)
          1
          0)))

(define (do-sub address config)
  ;(display "do-sub\n")
  ;(display "do-sub\n")
  (let ([accNum (bits->int (cdr (entry-value (car (conf-cpu config)))))]
        [memNum (bits->int (cdr (ram-read address (conf-ram config))))]
        [memMag (car (ram-read address (conf-ram config)))]
        [accMag (car (entry-value (car (conf-cpu config))))])
;    (display "aacNum: ")
;    (display accNum)
;    (display "\naccMag: ")
;    (display accMag)
;    (display "\nmemNum: ")
;    (display memNum)
;    (display "\nmemMag: ")
;    (display memMag)
;    (display "\n")
    (cond ((and (= accMag 1) (= memMag 1));both negative
;           (display "1\n")
;           (display (isPosSub accNum accMag memNum memMag))
           (conf (list
               (entry 'acc (negZer (cons (isPosSub accNum accMag memNum memMag) (checkACC (int->bits-width (abs (- accNum memNum)) 15)))))
               (entry 'pc (entry-value (cadr (conf-cpu config))))
               (entry 'rf (entry-value (caddr (conf-cpu config))))
               (entry 'aeb (isAEB (abs (- (- 0 accNum) (- 0 memNum))))))
               (conf-ram config)))
          ((= accMag 1);neg acc
           ;(display "2\n")
           (conf (list
               (entry 'acc (negZer (cons (isPosSub accNum accMag memNum memMag)(checkACC (int->bits-width (abs (- (- 0 accNum) memNum)) 15)))))
               (entry 'pc (entry-value (cadr (conf-cpu config))))
               (entry 'rf (entry-value (caddr (conf-cpu config))))
               (entry 'aeb (isAEB (abs (- (- 0 accNum) memNum)))))
               (conf-ram config)))
          ((= memMag 1);neg mem
           ;(display "3\n")
           (conf (list
               (entry 'acc (negZer (cons (isPosSub accNum accMag memNum memMag) (checkACC (int->bits-width (abs (- accNum (- 0 memNum))) 15)))))
               (entry 'pc (entry-value (cadr (conf-cpu config))))
               (entry 'rf (entry-value (caddr (conf-cpu config))))
               (entry 'aeb (isAEB (abs (- accNum (- 0 memNum))))))
               (conf-ram config)))
          (else
           ;(display "4\n")
           (conf (list
               (entry 'acc (negZer (cons (isPosSub accNum accMag memNum memMag) (checkACC (int->bits-width (abs (- accNum memNum)) 15)))))
               (entry 'pc (entry-value (cadr (conf-cpu config))))
               (entry 'rf (entry-value (caddr (conf-cpu config))))
               (entry 'aeb (isAEB (- accNum memNum))))
               (conf-ram config))))))

;determines the first element in the returned list
;if the numbers are equal, return 0
;if n1-n2 is positive return 1
;otherwise return 0
(define (isPosSub n1 mag1 n2 mag2)
  (let ([x (if (= mag1 1) (- 0 n1) n1)][y (if (= mag2 1) (- 0 n2) n2)])
  (if (= x y)
      0
      (if (> (- x y) 0)
          0
          1))))
;************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (do-input config)
; (do-output config)

; Each takes a TC-201 configuration and performs the appropriate action 
; (reading a number from the user or writing a number out to the user)
; and *returns* the resulting TC-201 configuration.

; For input, the new configuration has the value read in the 
; accumulator, and all other registers unchanged.
; To read in a value, you may use the following
; let construct:
; (let ((value (begin (display "input = ") (read)))) ...)

; To ensure the number typed by the user is in the correct range, 
; you may take its remainder on division by 2^(15).

; For output, the new configuration is returned *unchanged*.  
; If the integer value from the accumulator is in 
; value-from-accumulator, then the output to the user can be 
; produced by:

; (display "output = ")
; (display value-from-accumulator)
; (newline)

; Examples (these show how the interaction looks)

; The lines input = .. and output = .. show the interaction between 
; TC-201 and user.  The TC-201 configuration shows the value
; returned by the procedure.

;> (diff-configs config-ex1 (do-input config-ex1))
;input = 22
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0)))

;> (diff-configs config-ex1 (do-output config-ex1))
;output = 15
;'()

;************************************************************

(define (do-input config)
  ;(display "do-input\n")
  (let ((value (begin (display "input = ") (read))))
        (conf (list
               (entry 'acc (cons (isPosOne value)(int->bits-width (abs (remainder value 32768)) 15)))
               (entry 'pc (entry-value (cadr (conf-cpu config))))
               (entry 'rf (entry-value (caddr (conf-cpu config))))
               (entry 'aeb (entry-value (cadddr (conf-cpu config)))))
               (conf-ram config))))
;if the num is >=0, return 0
;otherwise, return 1
(define (isPosOne num)
  (if (>= num 0)
      0
      1))
;if number is negative return a negative sign
(define (isPosMag num)
  (if (> num 0)
      ""
      "-"))

(define (do-output config)
  ;(display "do-output\n")
  (let ([val (bits->int (cdr (entry-value (car (conf-cpu config)))))] [valMag (car (entry-value (car (conf-cpu config))))])
  (display "output = ")
    (if (= valMag 1)
        (display (- 0 val))
        (display val))       
  (newline))
  config)

;************************************************************
; ** problem 6 ** (10 points)
; Write four procedures

; (do-jump address config)
; (do-skipzero config)
; (do-skippos config)
; (do-skiperr config)

; (do-jump address config)
; takes a memory address and a TC-201 configuration, and
; returns a TC-201 configuration in which the program counter
; (pc) is set to the given address.  All other registers are
; unaffected.

; (do-skipzero config)
; takes a TC-201 configuration config and returns
; a TC-201 configuration in which the program counter (pc)
; is increased by 2 if the accumulator contains +0 or -0,
; and is increased by 1 otherwise.  All registers other than
; the pc are unaffected.

; (do-skippos config)
; takes a TC-201 configuration config and returns
; a TC-201 configuration in which the program counter (pc)
; is increased by 2 if the accumulator contains a nonzero
; positive number, and is increased by 1 otherwise.  
; All registers other than the pc are unaffected.

; (do-skiperr config)
; takes a TC-201 configuration config and returns
; a TC-201 configuration in which the program counter (pc)
; is increased by 2 if the arithmetic error bit contains 1
; and is increased by 1 if the arithmetic error bit contains 0.
; In either case, in the new configuration, the arithmetic
; error bit is set to 0.
; All registers other than the pc and the aeb are unaffected.

; Examples

;> (diff-configs config-ex1 (do-jump 5 config-ex1))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 0 1 0 1)))

;> (diff-configs config-ex2 (do-skipzero config-ex2))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 0)))

;> (diff-configs config-ex1 (do-skippos config-ex1))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 1)))

;> (diff-configs config-ex2 (do-skiperr config-ex2))
;'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 1)) (aeb (1) (0)))

;************************************************************

(define (do-jump address config)
  ;(display "do-jump\n")
  (conf (list
        (entry 'acc (entry-value (car (conf-cpu config))))
        (entry 'pc (int->bits-width (modulo address 4096) 12))
        (entry 'rf (entry-value (caddr (conf-cpu config))))
        (entry 'aeb (entry-value (cadddr (conf-cpu config)))))
        (conf-ram config)))

(define (do-skipzero config)
  ;(display "do-skipzero\n")
  ;(display (car (conf-cpu config)))
  (let ([acc (bits->int (cdr (entry-value (car (conf-cpu config)))))])
    ;(display acc)
  (if (equal? 0 acc)
      (conf (list
        (entry 'acc (entry-value (car (conf-cpu config))))
        (entry 'pc (int->bits-width (+ (bits->int (entry-value (cadr (conf-cpu config)))) 2) 12))
        (entry 'rf (entry-value (caddr (conf-cpu config))))
        (entry 'aeb (entry-value (cadddr (conf-cpu config)))))
        (conf-ram config))
      (conf (list
        (entry 'acc (entry-value (car (conf-cpu config))))
        (entry 'pc (int->bits-width (+ (bits->int (entry-value (cadr (conf-cpu config)))) 1) 12))
        (entry 'rf (entry-value (caddr (conf-cpu config))))
        (entry 'aeb (entry-value (cadddr (conf-cpu config)))))
        (conf-ram config)))))
     
(define (do-skippos config)
  ;(display "do-skippos\n")
  (if (= 0 (car (entry-value (car (conf-cpu config)))));if positive
      (conf (list
        (entry 'acc (entry-value (car (conf-cpu config))))
        (entry 'pc (int->bits-width (+ (bits->int (entry-value (cadr (conf-cpu config)))) 2) 12))
        (entry 'rf (entry-value (caddr (conf-cpu config))))
        (entry 'aeb (entry-value (cadddr (conf-cpu config)))))
        (conf-ram config))
      (conf (list
        (entry 'acc (entry-value (car (conf-cpu config))))
        (entry 'pc (int->bits-width (+ (bits->int (entry-value (cadr (conf-cpu config)))) 1) 12))
        (entry 'rf (entry-value (caddr (conf-cpu config))))
        (entry 'aeb (entry-value (cadddr (conf-cpu config)))))
        (conf-ram config))))

(define (do-skiperr config)
  ;(display "do-skiperr\n")
  (if (= 1 (car (entry-value (cadddr (conf-cpu config)))))
      (conf (list
        (entry 'acc (entry-value (car (conf-cpu config))))
        (entry 'pc (int->bits-width (+ (bits->int (entry-value (cadr (conf-cpu config)))) 2) 12))
        (entry 'rf (entry-value (caddr (conf-cpu config))))
        (entry 'aeb '(0)))
        (conf-ram config))
      (conf (list
        (entry 'acc (entry-value (car (conf-cpu config))))
        (entry 'pc (int->bits-width (+ (bits->int (entry-value (cadr (conf-cpu config)))) 1) 12))
        (entry 'rf (entry-value (caddr (conf-cpu config))))
        (entry 'aeb '(0)))
        (conf-ram config))))
           
;************************************************************
; ** problem 7 ** (10 points)
; Write two procedures

; (do-loadi address config)
; (do-storei address config)

; (loadi address config)
; takes a memory address and a TC-201 configuration and returns a TC-201 
; configuration that reflects the result of doing a "load indirect" from the
; given memory address to the accumulator.
; That is, the low-order 12 bits of the contents of the memory register 
; addressed by address are extracted and used as the memory address
; from which the contents are loaded into the accumulator.
; All other registers are unaffected.

; (storei address config)
; takes a memory address and a TC-201 configuration and returns a TC-201 
; configuration that reflects the result of doing a "store indirect" to the
; given memory address from the accumulator.
; That is, the low-order 12 bits of the contents of the memory register 
; addressed by address are extracted and used as the memory address
; to which the contents of the accumulator are copied.
; All other registers are unaffected.

; This example is useful for loadi and storei testing.

(define ram-ex3
  '((0 0 0 0  0 0 0 0  0 0 0 0  0 1 1 0)
    (1 1 1 1  0 0 0 0  0 0 0 0  0 1 0 1)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 1 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 0 0 0)
    (0 0 0 0  1 1 1 1  0 0 0 0  1 1 1 1)
    (0 1 0 1  0 1 0 1  0 1 0 1  0 1 0 1)))

(define config-ex3 (conf cpu-ex1 ram-ex3))

; Examples

;> (diff-configs config-ex3 (do-loadi 1 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)))

;> (diff-configs config-ex3 (do-loadi 2 config-ex3))
;'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1)))

;> (diff-configs config-ex3 (do-storei 1 config-ex3))
;'((5 (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)))

;> (diff-configs config-ex3 (do-storei 2 config-ex3))
;'((4 (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)))

;************************************************************
;eliminates 'num' many objects from the front of the list
;returns given list
(define (rightmost lst num)
  (cond ((= num 0) lst)
        (else (rightmost (cdr lst) (- num 1)))))
                   
(define (do-loadi address config)
  ;(display "do-loadi\n")
  (let* ([num (bits->int (rightmost (ram-read address (conf-ram config)) 4))]
         [cpy (ram-read num (conf-ram config))])
    (conf (list
        (entry 'acc cpy)
        (entry 'pc (entry-value (cadr (conf-cpu config))))
        (entry 'rf (entry-value (caddr (conf-cpu config))))
        (entry 'aeb (entry-value (cadddr (conf-cpu config)))))
        (conf-ram config))))
  
(define (do-storei address config)
  ;(display "do-storei\n")
  (let* ([num (bits->int (rightmost (ram-read address (conf-ram config)) 4))])
    (conf
     (conf-cpu config);aeb?
     (ram-write num (entry-value (car (conf-cpu config))) (conf-ram config)))))
;************************************************************
; ** problem 8 ** (10 points)
; Write one procedure

; (next-config config)

; that takes a TC-201 configuration and returns the next TC-201 configuration,
; after one iteration of the fetch/execute cycle.

; If the run flag (rf) is 0, then the configuration config is returned unchanged,
; because the machine is halted.

; The instructions that should be implemented are:

;halt,    0000
;load,    0001
;store,   0010
;add,     0011
;sub,     0100
;input,   0101
;output,  0110
;jump     0111
;skipzero,1000
;skippos, 1001
;skiperr, 1010
;loadi,   1011
;storei.  1100

; You should intepret an undefined opcode  (1101 through 1111) 
; as a halt instruction.

; For a halt instruction, in the returned configuration the run flag is 0 and all
; other registers are unchanged.

; Otherwise, the program counter (pc) contains a memory address, and the TC-201 
; instruction at that location is fetched and executed, and the resulting 
; configuration is returned.  Note that all instructions result in a configuration
; being returned, even input and output.

; This example is useful for testing next-config.

(define cpu-ex4
  (list
   (entry 'acc '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
   (entry 'pc  '(0 0 0 0  0 0 0 0  0 0 0 0))
   (entry 'rf '(1))
   (entry 'aeb '(0))))

(define ram-ex4
  '((0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1)
    (0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 1  0 1 0 1)
    (1 1 1 1  1 1 1 1  0 0 0 0  0 0 0 0)))

(define config-ex4 (conf cpu-ex4 ram-ex4))

; Examples
; (Your configurations may be equivalent.)

;> (next-config config-ex4)
;(conf
; (list
;  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;  (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 1))
;  (entry 'rf '(1))
;  (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;   (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;> (next-config (next-config config-ex4))
;(conf
; (list
;  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;  (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
;  (entry 'rf '(1))
;  (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))

;> (next-config (next-config (next-config config-ex4)))
;(conf
; (list
;  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;  (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
;  (entry 'rf '(0))
;  (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;   (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))

;************************************************************
;extracts the first four numbers from a list
(define (firstfour lst num new)
  (cond ((= num 0) (reverse new))
        (else (firstfour (cdr lst) (- num 1) (cons (car lst) new)))))

;increases the counter by 1
(define (inc_Count config)
  (conf (list
         (entry 'acc (entry-value (car (conf-cpu config))))
         (entry 'pc (int->bits-width (+ 1 (bits->int (entry-value (cadr (conf-cpu config))))) 12))
         (entry 'rf (entry-value (caddr (conf-cpu config))))
         (entry 'aeb (entry-value (cadddr (conf-cpu config)))));aeb?
         (conf-ram config)))

(define (next-config config)
  (let* ([add1 (bits->int (entry-value (cadr (conf-cpu config))))]
         [op (bits->int (firstfour (ram-read add1 (conf-ram config)) 4 '()))]
         [counter (bits->int (rightmost (ram-read add1 (conf-ram config)) 4 ))])
  (cond ((equal? 0 (entry-value (caddr (conf-cpu config)))) config)
        ((= op 0)
         (conf (list
                (entry 'acc (entry-value (car (conf-cpu config))))
                (entry 'pc (entry-value (cadr (conf-cpu config))))
                (entry 'rf '(0))
                (entry 'aeb (entry-value (cadddr (conf-cpu config)))));aeb?
               (conf-ram config)))
        ((= op 1) (inc_Count (do-load counter config)))
        ((= op 2) (inc_Count (do-store counter config)))
        ((= op 3) (inc_Count (do-add counter config)))
        ((= op 4) (inc_Count (do-sub counter config)))
        ((= op 5) (inc_Count (do-input config)))
        ((= op 6) (inc_Count (do-output config)))
        ((= op 7) (do-jump counter config))
        ((= op 8) (do-skipzero config))
        ((= op 9) (do-skippos config))
        ((= op 10)(do-skiperr config))
        ((= op 11)(inc_Count (do-loadi counter config)))
        ((= op 12)(inc_Count (do-storei counter config)))
        (else (conf (list
                (entry 'acc (entry-value (car (conf-cpu config))))
                (entry 'pc (entry-value (cadr (conf-cpu config))))
                (entry 'rf '(0))
                (entry 'aeb (entry-value (cadddr (conf-cpu config)))));aeb?
               (conf-ram config))))))

;************************************************************
; ** problem 9 ** (10 points)
; Write three procedures

; (init-config lst)
; (symbol-table prog)
; (assemble prog)

; (init-config lst)
; takes a list lst 16 bit patterns, and returns a TC-201 configuration 
; in which those patterns are loaded into RAM starting with address 0, 
; and the CPU registers are initialized so that the accumulator has
; value +0, the program counter has address 0, the run flag has 
; value 1, and the arithmetic error bit has value 0.

; (symbol-table prog)
; takes a TC-201 assembly language program prog (in the format specified below) 
; and returns a table of entries in which the key is a symbol that is a label 
; in prog and the value is the corresponding memory address for that
; instruction or data value (when the program is loaded into memory starting 
; at address 0.)  

; The addresses in the table should be in increasing order.

; (assemble prog)
; translates a TC-201 assembly language program prog 
; into a list of 16-bit patterns to be loaded into the TC-201 memory.

; The symbolic opcodes are:
; halt, load, store, add, sub, input, output
; jump, skipzero, skippos, skiperr, loadi, storei.

; There is also a data statement.

; An assembly language program is a list of "lines", where
; each line is a list of two or three elements representing
; an instruction or a data statment.
; If the line has three elements, the first one is a symbolic label that
; should appear in the symbol table for the program.
; The remaining two elements (or the only two elements,
; if the line has just two elements) are either a symbol
; representing an opcode and an address, or the symbol 'data
; and a data value.  The address field of an instruction may
; be a number in the range 0 to 4095 inclusive, or a symbolic
; label, in which case the address is the numeric value of the
; label in the symbol table.  The value field of a data statement
; may be a number in the range -32767 to 32767 inclusive, or
; a symbolic label, in which case the value used is the numeric
; value of the label in the symbol table.

; You may assume that numeric addresses and data values will
; be in the correct ranges.

; Note that even instructions like halt, input, and skipzero, which
; ignore their address fields, must have an address specified.
; (A typical choice is 0 for the address fields of such instructions.)

; Example TC-201 assembly language programs

; a program with only instructions, numeric addresses, and no labels

(define prog1
  '((load 3)
    (store 4)
    (halt 0)))


; a program with only data statements, three labels, and both numeric
; and symbolic data values

(define prog2
  '((x data 1)
    (y data -2)
    (z data y)))

; a version of the program we wrote in lecture to sum up
; a zero-terminated sequence of numbers, output the sum, and halt.

(define prog3
  '((start  load constant-zero)
   (        store sum)
   (next    input 0)
   (        skipzero 0)
   (        jump add-num)
   (        load sum)
   (        output 0)
   (        halt 0)
   (add-num add sum)
   (        store sum)
   (        jump next)
   (sum     data 0)
   (constant-zero data 0)))

; Examples of init-config, symbol-table and assemble
;> (init-config ram-ex2)
;(conf
; (list (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
;       (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0)) 
;       (entry 'rf '(1)) 
;       (entry 'aeb '(0)))
; '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1)
;   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;   (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0)))

;> (symbol-table prog1)
;'()

;> (symbol-table prog2)
;(list (entry 'x 0) (entry 'y 1) (entry 'z 2))

;> (symbol-table prog3)
;(list
; (entry 'start 0)
; (entry 'next 2)
; (entry 'add-num 8)
; (entry 'sum 11)
; (entry 'constant-zero 12))


;> (assemble prog1)
;'((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

;> (assemble prog2)
;'((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
;  (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

;> (assemble prog3)
;'((0 0 0 1 0 0 0 0 0 0 0 0 1 1 0 0)
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
;  (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 1 1 1 0 0 0 0 0 0 0 0 1 0 0 0)
;  (0 0 0 1 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

;************************************************************

(define (init-config lst)
    (conf (list
        (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
        (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0))
        (entry 'rf '(1))
        (entry 'aeb '(0)))
        lst))

(define (symbol-table prog)
  (sym-tab-HELP prog 0 '()))

;does what symbol-table is supposed to do, but has added variables to keep track of the current row #.
;and variable, num where the list that will be returned is stored
;this makes the procedure tail recusive
(define (sym-tab-HELP prog num new)
  (cond ((empty? prog) new)
        ((= 3 (length (car prog))) (sym-tab-HELP (cdr prog) (+ num 1) (append new (list (entry (car (car prog)) num)))))
        (else (sym-tab-HELP (cdr prog) (+ num 1) new))))
                
(define (assemble prog)
  (ass-assist prog (symbol-table prog)))

;does exactly what assemble is supposed to , but takes in a symbol-table, so that as enties are eliminated, the generated symbol table is not incomplete
(define (ass-assist prog symtab)
    (cond ((empty? prog) '())
          ((= 3 (length (car prog)))
           (if (number? (last (car prog)))
               (cons (lookupNeg (append (ass-HELP (car (cdr (car prog)))) (int->bits-width (last (car prog)) 12)) '() 0) (ass-assist (cdr prog) symtab))
               (cons (lookupNeg (append (ass-HELP (car (cdr (car prog)))) (int->bits-width (lookup (last (car prog)) symtab) 12)) '() 0) (ass-assist (cdr prog) symtab))))
          ((= 2 (length (car prog)))
           (if (number? (car (cdr (car prog))))
               (cons (lookupNeg (append (ass-HELP (car (car prog))) (int->bits-width (car (cdr (car prog))) 12)) '() 0)(ass-assist (cdr prog) symtab))
               (cons (lookupNeg(append (ass-HELP (car (car prog))) (int->bits-width (lookup (car (cdr (car prog))) symtab) 12))'() 0) (ass-assist (cdr prog) symtab))))))

;if any value is supposed to be negative, this method makes the first item in the list a one, instead of a zero
;does this by consing 1 onto cdr of the list
(define (lookupNeg progchange otherprog num)
  (cond ((empty? progchange)
         (if (= num 1)
             (cons 1 (cdr (reverse otherprog)))
             (reverse otherprog)))
        ((= -1 (car progchange)) (lookupNeg (cdr progchange) (cons 1 otherprog) 1))
        (else (lookupNeg (cdr progchange) (cons (car progchange) otherprog) num))))

;looksup a key and returns its value
(define (lookup key table)
  (cond ((empty? table) 0)
        ((equal? (entry-key (car table)) key) (entry-value (car table)))
        (else (lookup key (cdr table)))))

;stores all possible variables for the differnt funtions
;returns lst that matches var
(define (ass-HELP var)
 (cond ((equal? var 'halt) '(0 0 0 0))
       ((equal? var 'load) '(0 0 0 1))
       ((equal? var 'store) '(0 0 1 0))
       ((equal? var 'add) '(0 0 1 1))
       ((equal? var 'sub) '(0 1 0 0))
       ((equal? var 'input) '(0 1 0 1))
       ((equal? var 'output) '(0 1 1 0))
       ((equal? var 'jump) '(0 1 1 1))
       ((equal? var 'skipzero) '(1 0 0 0))
       ((equal? var 'skippos) '(1 0 0 1))
       ((equal? var 'skiperr) '(1 0 1 0))
       ((equal? var 'loadi) '(1 0 1 1))
       ((equal? var 'storei) '(1 1 0 0))
       (else '(0 0 0 0))))

       
;************************************************************
; ** problem 10 ** (10 points)
; Write one procedure and one program for the TC-201

; (simulate n config)
; remainder-prog

; (simulate n config)
; simulates the TC-201 computer from the configuration config until
; either it halts (the run flag is 0) or n iterations of the fetch/execute
; cycle have been performed, whichever is first.
; The result returned should be a list of the successive configurations 
; of the TC-201 starting with the config.

; You are strongly advised to use your simulate procedure to help you test 
; your implementation of the instructions more extensively than the test cases 
; in the assignment.

; remainder-prog
; reads in two positive integers from the user, prints out the remainder 
; when the first is divided by the second, and then halts.
; (Hint: division can be viewed as repeated subtraction.)

; Examples 
; (This program stops after executing 3 instructions, returning
; 4 configurations, including the initial one.)

;> (simulate 5 config-ex4)
;(list
; (conf
;  (list
;   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 'rf '(1))
;   (entry 'aeb '(0)))
;  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;    (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))
; (conf
;  (list
;   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 1))
;   (entry 'rf '(1))
;   (entry 'aeb '(0)))
;  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;    (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))
; (conf
;  (list
;   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
;   (entry 'rf '(1))
;   (entry 'aeb '(0)))
;  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))
; (conf
;  (list
;   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
;   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
;   (entry 'rf '(0))
;   (entry 'aeb '(0)))
;  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
;    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))))

; Examples of run-time behavior of remainder-prog interacting 
; with user.  We "capture" the returned list of configurations 
; by assigning it to be the value of the symbol results.

;> (define results (simulate 100 (init-config (assemble remainder-prog))))
;input = 27
;input = 6
;output = 3

;> (define results (simulate 100 (init-config (assemble remainder-prog))))
;input = 69
;input = 23
;output = 0

;************************************************************

(define (simulate n config)
  (sim-HELP n config (list config)))

;does what simulate is supposed to but with added variable new,
;so that the method is tail recursive
(define (sim-HELP n config new)
  (let ([x (next-config config)])
  (cond ((= n 0) new)
        ((equal? (car (entry-value (caddr (conf-cpu config)))) 0) new)
        (else  (sim-HELP (- n 1) x (append new (list x)))))))

(define remainder-prog
  '((start  input 0)
   (        store x)
   (        input 0)
   (        store y)
   (        jump next)
   (next    load x)
   (        skippos 0)
   (        jump final)
   (        jump sub-num)
   (final   skipzero 0)
   (        jump neg)
   (        output 0)
   (        halt 0)
   (neg     add y)
   (        output 0)
   (        halt 0)
   (sub-num sub y)
   (        store x)
   (        jump next)
   (x       data 0)
   (y       data 0)))

;************************************************************
; ** problem 11 ** (10 points)
; Write one program for the TC-201

; reverse-prog

; that reads in a zero-terminated sequence of numbers from
; the user, prints them out in reverse, and halts.
; The terminating 0 is not printed out.
; You need not worry about running out of memory.

; Examples

; Example of run-time behavior of reverse-prog interacting with user.
; We "capture" the sequence of configurations returned
; by simulate by making it the value of the symbol results.

;> (define results (simulate 100 (init-config (assemble reverse-prog))))
;input = 13
;input = -44
;input = 16
;input = 0
;output = 16
;output = -44
;output = 13
;************************************************************

(define reverse-prog
  '((read-num  input 0)
    (          skipzero 0)
    (          jump store-num)
    (          jump print)
    (print     load pointer)
    (          sub constant-one)
    (          store pointer)
    (          loadi pointer)
    (          skipzero 0)
    (          jump p2)
    (          halt 0)
    (p2        output 0)
    (          jump print)
    (store-num storei pointer)
    (          load pointer)
    (          add constant-one)
    (          store pointer)
    (          jump read-num)
    (pointer   data table)
    (constant-one  data 1)
    (constant-zero data 0)
    (table     data 0)))

; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    '***OK***
			    '====X====)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(test 'hours hours (lambda (x) (> x 0)))

(test 'ram-read (ram-read 0 ram-ex1) '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))

(test 'ram-read (ram-read 6 ram-ex2) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(test 'diff-rams (diff-rams ram-ex1 ram-ex2) '((1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1)) (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0))))

(test 'diff-rams (diff-rams '() '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))) '())

(test 'diff-rams (diff-rams ram-ex1 (ram-write 2 '(0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1) ram-ex1)) '((2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1))))

(test 'diff-rams (diff-rams ram-ex2 (ram-write 5 '(1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0) ram-ex2)) '((5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0))))

(test 'diff-rams (diff-rams ram-ex1 (ram-write 1 '(0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1) ram-ex1)) '((1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1))))


(test 'extract (extract 1 3 '(a b c d e)) '(b c d))

(test 'extract (extract 4 4 '(a b c d e)) '(e))

(test 'bits->int (bits->int '(0)) 0)

(test 'bits->int (bits->int '(0 0 0 1 1 0)) 6)

(test 'int->bits (int->bits 0) '(0))

(test 'int->bits (int->bits 6) '(1 1 0))

(test 'int->bits (int->bits-width 14 8) '(0 0 0 0 1 1 1 0))

(test 'int->bits (int->bits-width 14 3) "field too small")

(test 'diff-configs (diff-configs config-ex1 config-ex2) '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (aeb (0) (1))
  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
  (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0))))

(test 'incr-pc (incr-pc 1 config-ex1)
(conf
 (list
  (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
  (entry 'pc '(0 0 0 0 0 0 0 0 1 0 0 0))
  (entry 'rf '(1))
  (entry 'aeb '(0)))
 '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0)
   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(test 'diff-configs (diff-configs config-ex2 (incr-pc 4090 config-ex2))
'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1))))

(test 'load-store (diff-configs config-ex1 (do-load 1 config-ex1))
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))))

(test 'load-store (diff-configs config-ex2 (do-load 12 config-ex2))
'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(test 'load-store (diff-configs config-ex1 (do-store 5 config-ex1))
'((5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))))

(test 'load-store  (diff-configs config-ex2 (do-store 0 config-ex2))
'((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1) (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))))


(test 'add-sub (diff-configs config-ex1 (do-add 3 config-ex1))
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1))))

(test 'add-sub (diff-configs config-ex2 (do-add 3 config-ex2))
'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1))
  (aeb (1) (0))))

(test 'add-sub (diff-configs config-ex1 (do-sub 3 config-ex1))
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1))))

(test 'add-sub (diff-configs config-ex2 (do-sub 3 config-ex2))
'((acc (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) (1 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1))
  (aeb (1) (0))))

(test 'add-sub  (let ((config (conf cpu-ex1 '((0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))) (diff-configs config (do-add 0 config)))
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (aeb (0) (1))))


(test 'skip-jump (diff-configs config-ex1 (do-jump 5 config-ex1))
'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 0 1 0 1))))

(test 'skip-jump (diff-configs config-ex2 (do-skipzero config-ex2))
'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 0))))

(test 'skip-jump (diff-configs config-ex1 (do-skippos config-ex1))
'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 1))))

(test 'skip-jump (diff-configs config-ex2 (do-skiperr config-ex2))
'((pc (0 0 0 0 0 0 0 0 0 1 1 1) (0 0 0 0 0 0 0 0 1 0 0 1)) (aeb (1) (0))))


(test 'loadi-storei (diff-configs config-ex3 (do-loadi 1 config-ex3))
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1))))

(test 'loadi-storei (diff-configs config-ex3 (do-loadi 2 config-ex3))
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1))))

(test 'loadi-storei (diff-configs config-ex3 (do-storei 1 config-ex3))
'((5 (0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))))

(test 'loadi-storei (diff-configs config-ex3 (do-storei 2 config-ex3))
'((4 (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))))


(test 'next-config (next-config config-ex4)
 (conf
  (list
   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 1))
   (entry 'rf '(1))
   (entry 'aeb '(0)))
  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
    (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0))))

(test 'next-config (next-config (next-config config-ex4))
 (conf
  (list
   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
   (entry 'rf '(1))
   (entry 'aeb '(0)))
  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))))

(test 'next-config (next-config (next-config (next-config config-ex4)))
 (conf
  (list
   (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
   (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
   (entry 'rf '(0))
   (entry 'aeb '(0)))
  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))))



(test 'init-config (init-config ram-ex2)
 (conf
  (list (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
        (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0)) 
        (entry 'rf '(1)) 
        (entry 'aeb '(0)))
  '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
    (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1)
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0))))

(test 'symbol-table (symbol-table prog1)
 '())

(test 'symbol-table (symbol-table prog2)
 (list (entry 'x 0) (entry 'y 1) (entry 'z 2)))

(test 'symbol-table (symbol-table prog3)
 (list
  (entry 'start 0)
  (entry 'next 2)
  (entry 'add-num 8)
  (entry 'sum 11)
  (entry 'constant-zero 12)))

(test 'assemble (assemble prog1)
 '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
   (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(test 'assemble (assemble prog2)
 '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)
   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))

(test 'assemble (assemble prog3)
 '((0 0 0 1 0 0 0 0 0 0 0 0 1 1 0 0)
   (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
   (0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
   (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 1 1 1 0 0 0 0 0 0 0 0 1 0 0 0)
   (0 0 0 1 0 0 0 0 0 0 0 0 1 0 1 1)
   (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 1)
   (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
   (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0)
   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(test 'simulate (simulate 5 config-ex4)
 (list
  (conf
   (list
    (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 0))
    (entry 'rf '(1))
    (entry 'aeb '(0)))
   '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
     (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
     (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))
  (conf
   (list
    (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
    (entry 'pc '(0 0 0 0 0 0 0 0 0 0 0 1))
    (entry 'rf '(1))
    (entry 'aeb '(0)))
   '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
     (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
     (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))
  (conf
   (list
    (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
    (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
    (entry 'rf '(1))
    (entry 'aeb '(0)))
   '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
     (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
     (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))
  (conf
   (list
    (entry 'acc '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1))
    (entry 'pc '(0 0 0 0 0 0 0 0 0 0 1 0))
    (entry 'rf '(0))
    (entry 'aeb '(0)))
   '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
     (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)
     (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)))))

(test 'add-sub (bits->int (cdr (entry-value (car (conf-cpu (do-sub 0 config-ex-neg)))))) 539)
(test 'add-sub (bits->int (cdr (entry-value (car (conf-cpu (do-sub 1 config-ex-neg)))))) 569)
(test 'add-sub (bits->int (cdr (entry-value (car (conf-cpu (do-sub 2 config-ex-neg)))))) 0)
(test 'add-sub (bits->int (cdr (entry-value (car (conf-cpu (do-sub 3 config-ex-neg)))))) 30)
(test 'add-sub (bits->int (cdr (entry-value (car (conf-cpu (do-sub 4 config-ex-neg)))))) 32752)
(test 'add-sub (bits->int (cdr (entry-value (car (conf-cpu (do-sub 5 config-ex-neg)))))) 0)

(test 'add-sub (bits->int (cdr (entry-value (car (conf-cpu (do-sub 0 config-ex-pos)))))) 569)
(test 'add-sub (bits->int (cdr (entry-value (car (conf-cpu (do-sub 1 config-ex-pos)))))) 539)
(test 'add-sub (bits->int (cdr (entry-value (car (conf-cpu (do-sub 2 config-ex-pos)))))) 30)
(test 'add-sub (bits->int (cdr (entry-value (car (conf-cpu (do-sub 3 config-ex-pos)))))) 0)
(test 'add-sub (bits->int (cdr (entry-value (car (conf-cpu (do-sub 4 config-ex-pos)))))) 0)
(test 'add-sub (bits->int (cdr (entry-value (car (conf-cpu (do-sub 5 config-ex-pos)))))) 32752)

(test 'add-sub (diff-configs config-ex-neg (do-sub 0 config-ex-neg))
      '((acc (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 1 0 0 0 0 1 1 0 1 1))))
(test 'add-sub (diff-configs config-ex-neg (do-sub 1 config-ex-neg))
      '((acc (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 0 0 0 0 0 1 0 0 0 1 1 1 0 0 1))))
(test 'add-sub (diff-configs config-ex-neg (do-sub 2 config-ex-neg))
      '((acc (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
(test 'add-sub (diff-configs config-ex-neg (do-sub 3 config-ex-neg))
      '((acc (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0))))
(test 'add-sub (diff-configs config-ex-neg (do-sub 4 config-ex-neg))
      '((acc (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0))))
(test 'add-sub (diff-configs config-ex-neg (do-sub 5 config-ex-neg))
      '((acc (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) (aeb (0) (1))))

(test 'add-sub (diff-configs config-ex-pos (do-sub 0 config-ex-pos))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 1 0 0 0 1 1 1 0 0 1))))
(test 'add-sub (diff-configs config-ex-pos (do-sub 1 config-ex-pos))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 0 0 0 0 0 1 0 0 0 0 1 1 0 1 1))))
(test 'add-sub (diff-configs config-ex-pos (do-sub 2 config-ex-pos))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0))))
(test 'add-sub (diff-configs config-ex-pos (do-sub 3 config-ex-pos))
     '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
(test 'add-sub (diff-configs config-ex-pos (do-sub 4 config-ex-pos))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) (aeb (0) (1))))
(test 'add-sub (diff-configs config-ex-pos (do-sub 5 config-ex-pos))
      '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) (1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0))))
;********************** end of hw6.scm **********************
