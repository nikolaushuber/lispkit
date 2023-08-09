(letrec append 
    (append lambda (x y) 
        (if (eq x (quote nil))
            y 
            (cons (car x) (append (cdr x) y))
        )
    )
)
