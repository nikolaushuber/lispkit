(letrec factorial
    (factorial lambda (x) 
        (if 
            (eq x (quote 1)) 
            (quote 1)
            (mul x (factorial (sub x (quote 1))))
        )
    )
)

