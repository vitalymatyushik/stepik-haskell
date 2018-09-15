module NonstrictEvaluation where
    -- 1) Redux count for `const $ const (4 + 5) $ max 42`
    -- in case if we have the following definitions
    -- id x = x
    -- const x y = x
    -- max x y = if x <= y then y else x
    -- infixr 0 $
    -- f $ x = f x
    -- ???
    -- Answer: 3

    -- 2) Steps of reduction with devision mechanism for expressions 
    bar x y z = x + y
    foo a b = bar a a (a + b)
    value = foo (3 * 10) (5 - 2)
    -- Answer: 4

    -- 3) Non-diverged functions from the list:
    foo' a = a
    bar' = const foo
    baz x = const True
    quux = let x = x in x
    corge = "Sorry, my value was changed"
    grault x 0 = x
    grault x y = x
    garply = grault 'q'
    waldo = foo
    -- Answer: 
    --  corge, 
    --  baz

    -- 4) Choose expressions that are in WHNF:
    -- \x -> x
    -- 3
    -- fst (1,0)
    -- [undefined, 4 + 5, -1]
    -- (+) (2 * 3 * 4)
    -- (,) undefined
    -- Answer: 
    --      [undefined, 4 + 5, -1] 
    --      (+) (2 * 3 * 4), 
    --      (,) undefined

    -- 5) Which expressions were improved: using `seq`
    foo'' 0 x = x
    foo'' n x = let x' = foo'' (n - 1) (x + 1)
            in x' `seq` x'

    bar'' 0 f = f
    bar'' x f = let f' = \a -> f (x + a) 
                    x' = x - 1
                in f' `seq` x' `seq` bar'' x' f'

    baz'' 0 (x, y) = x + y
    baz'' n (x, y) = let x' = x + 1
                         y' = y - 1
                         p  = (x', y')
                         n' = n - 1
                     in p `seq` n' `seq` baz'' n' p

    quux'' 0 (x, y) = x + y
    quux'' n (x, y) = let x' = x + 1
                          y' = y - 1
                          p  = (x', y')
                          n' = n - 1
                      in x' `seq` y' `seq` n' `seq` quux'' n' p
    -- Answer: quux''