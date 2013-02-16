(* Plain mutually recursive support AND keyword *)
fun isEven x = if x = 0 then true else isOdd (x - 1)
and isOdd x = if x = 0 then false else isEven (x - 1)

(* Mutually recursive datatype *)
datatype CircleShape =
         Circle of int * int
       | RectangleWrapper of RectangleShape
     and RectangleShape =
         Rectangle of int * int
       | CircleWrapper of CircleShape

(* Workaround *)
fun earlier (f, x) = f x
fun later x = earlier(later, x)
