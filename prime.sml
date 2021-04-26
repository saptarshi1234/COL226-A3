fun mod_internal x =
  fn y => if x < y then x else ((mod_internal (x - y)) y) ;

fun noFactorsAboveOf x = fn i => 
  if (i * 2) > x then true 
  else not(((mod_internal x) i) = 0) andalso ((noFactorsAboveOf x) (i + 1)) 


fun prime x = ((noFactorsAboveOf x) 2);

fun getKthPrime k = fn n => fn i => 
  if i = k then n - 1 else
    let val ii = if (prime n) then (i + 1) else i  
    in (((getKthPrime k) (n + 1)) ii) 
    end


fun kthPrime k = (((getKthPrime k) 2) 0)
val x = kthPrime(2)

val args = CommandLine.arguments()
val v = kthPrime (case Int.fromString(hd args) of SOME(n) => n | NONE => raise Fail(""))
val _ = (print ((Int.toString v) ^ "\n"); OS.Process.exit(OS.Process.success))