#lang racket

(define ABB '(8
              (5
               (2  ()  ())
               (7  ()  ()))
              (9
               ()
               (15
                (11  ()  ())
                () ))))


(define ABB2 '(a (b (c () ()) ()) (d (e () ()) ())))

(define g'(
           (A (B 2) (D 10))
           (B (C 9) (E 5))
           (C (A 12) (D 6))
           (D (E 7))
           (E (C 3))))

; max: Recibe un árbol y devuelve el valor máximo entre los nodos
(define (max tree)
  (cond
    [(null? tree) '()]
    [(not (null? (caddr tree))) (max (caddr tree))]
    [else (car tree)]
    )
  )

; min: Recibe un árbol y devuelve el valor mínimo entre los nodos
(define (min tree)
  (cond
    [(null? tree) '()]
    [(not (null? (cadr tree))) (min (cadr tree))]
    [else (car tree)]
    )
  )

; rango: Recibe un árbol binario y devuelve el máximo y mínimo valores entre los nodos
(define (rango tree)
  (list (min tree) (max tree))
  )


; cuenta-nivel-aux: Recibe un árbol, nivel deseado y nivel actual. Devuelve la cantidad de nodos en el nivel deseado
(define (cuenta-nivel-aux tree level current-level)
  (cond
    [(null? tree) 0]
    [(= current-level level) 1]
    [else (+
           (cuenta-nivel-aux (cadr tree) level (+ current-level 1))
           (cuenta-nivel-aux (caddr tree) level (+ current-level 1)))]
    )
  )

; cuenta-nivel: Recibe un árbol y un nivel deseado. Devuelve la cantidad de nodos en el nivel
(define (cuenta-nivel n tree)
  (cuenta-nivel-aux tree n 0)
  )



; nodos-destino: Recibe un grafo y un identificador. Devuelve la cantidad de nodos destino directo del nodo con el identificador
(define (nodos-destino g id)
   (apply append (map (lambda (x)
           (cond
             [(eq? (car x) id)
              (map (lambda (y) (car y)) (cdr x))]
             [else '()])) g))
  )

; elimina-nodo: Elimina un nodo a partir de un identificador dado
(define (elimina-nodo g id)
  (apply append
         (map (lambda (x)
                (if (not(eq? (car x) id))
                    (list (append (list (car x)) (apply append (map (lambda (y) (if (not(eq? (car y) id)) (list y) '())) (cdr x)))))
                    '() )) g))
  )


