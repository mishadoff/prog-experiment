(* hello-world *)
fun print_hello_world () = print "Hello, world\n";

(* variables *)
val a = 38;
val b = a + 3;
val c = b + 1;

(* conditionals *)
val cond = if 5 > 4 then 5 else 4

val cond2 = if 5 > 4 andalso 4 > 5 then 5 else 7

(* functions *)
fun square x = x * x

fun sum (x, y, z) = x + y + z

(* function caclulate square of number *)
fun cube x = x * x * x

(* tuples *)
fun swap (x, y) = (y, x)

fun get_month (date : int * string * int) : string = (#2 date)
val month1 = get_month(2012, "January", 21);

(* records *)
fun get_month_real ({year, month, day}) = month
val month2 = get_month_real({year=2012, month="January", day=21});

(* lexical scope *)
fun add3 x =
    let
        val num = 3
        fun add_to x = x + num
    in
        add_to x
    end


(* function composition *)
val cube_and_cube = cube o cube

val another_cube = cube;

fun binary_apply (f, x, y) = f (x, y)
val res = binary_apply(swap, 1, 2);

(* lambdas *)
val lambda_res = binary_apply(fn (x, y) => x * y, 23, 46)

fun factorial x =
    case x of
        0 => 1
      | _ => x * factorial(x - 1)

fun concat (x, y) =
    case x of
        [] => y
      | x::xs => x::concat(xs, y)

(* datatypes *)

datatype 'a list = EMPTY | CONS of 'a * 'a list
val list = CONS(1, CONS(2, CONS(3, EMPTY)))
