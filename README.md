Lambda: A lambda-calculus reducer/evaluator
===========================================

Try out Church Numerals:

    \> 0 = \f.\x.x
    \f.\x.x
    \> S = \n.\f.\x.f(nfx)
    \n.\f.\x.f(nfx)
    \> 1 = S0
    \f.\x.fx
    \> 2 = S1
    \f.\x.f(fx)
    \> 3 = S2
    \f.\x.f(f(fx))
    \> + = \n.\m.nSm
    \n.\m.n(\n'.\f.\x.f(n'fx))m
    \> + 2 3
    \f.\x.f(f(f(f(fx))))
    \> + 2 1
    \f.\x.f(f(fx))
    \> 3
    \f.\x.f(f(fx))
    \> * = \n.\m.n(+m)0
    \n.\m.n(\m'.m(\n'.\f.\x.f(n'fx))m')(\f.\x.x)
    \> * 3 2
    \f.\x.f(f(f(f(f(fx)))))
    \> * 2 2
    \f.\x.f(f(f(fx)))

