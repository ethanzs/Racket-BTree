; BTree
; Version 1.0
; Author: Ethan Seligman
; Searching a BTree recursively

; BTree definition
(define tree ' ( "R" 100 999     
(       
    ("R" 100 199 
        (
            ("L" 120 140 160 180)
        )
    )       
    ("R" 200 299 
        (
            ("L" 220 240 260 280)
        )
    )
)))

; Helper Functions
(define (get-child root)
  (if (null? (cadddr root))
      '(0)
      (cadddr root)
  )
)

(define (in-base-value? tree value)
  (if (<= (cadr tree) value)
      (if (>= (caddr tree) value)
          #t
          #f
      )
      #f
  )
)

(define (in-leaf-value? leaf value)
  (if(null? leaf)
     #f
     (if (= value (car leaf))
         #t
         (in-leaf-value? (cdr leaf) value)
     )
  )
)

(define (in-root-value root value)
  (if (null? root)
      #f
      (if (in-base-value? (car root) value)
             (in-leaf-value? (cdr(car(cadddr(car root)))) value)
             (in-root-value (cdr root) value)
      )
  )
)

; Main Search Function
(define (search tree value)
  (if (in-base-value? tree value)
      (in-root-value (get-child tree) value)
      #f
  )
)

; "Driver"
(search tree 221)