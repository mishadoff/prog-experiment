(* simplest factorial solution *)
fun factorial n =
    if n = 0 then 1 else n * factorial(n - 1)

(* calculates x to nth power *)
fun pow (x, n) =
    if n = 0 then 1 else x * pow(x, n - 1)

fun selfpow x = pow (x, x)
fun square x = pow(x, 2)
fun cube x = pow(x, 3)

(* function composition *)
val square_of_selfpow = square o selfpow

(* list length *)
fun length xs =
    case xs of
        [] => 0
      | x::xs' => 1 + (length xs')

(* lists concatenation *)
fun concat (xs, ys) =
    case xs of
        [] => ys
      | x::xs' => x::concat(xs', ys)

(* first class function *)
val adder100 = fn x => x + 100

(* another first class function *)
val swapper = fn (x, y) => (y, x)


(* some practical functions *)
val start_from_capital = List.filter(fn x => Char.isUpper(String.sub (x, 0)))
val string_counts = List.map(fn x => String.size(x))
val longest_string = List.foldl(fn (x, y) => if (String.size x) > (String.size y) then x else y) ""
