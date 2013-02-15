(* simplest factorial solution *)
fun factorial n =
    if n = 0 then 1 else n * factorial(n - 1)

(* calculates x to nth power *)
fun pow (x, n) =
    if n = 0 then 1 else x * pow(x, n - 1)

fun selfpow x = pow (x, x)
fun square x = pow(x, 2)
fun cube x = pow(x, 3)
