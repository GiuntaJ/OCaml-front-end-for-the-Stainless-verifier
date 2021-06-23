import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub116 {
  /* Ex 3. Iterator */
  def iter(((n, f)), x) = { if (n eq 1) f(x) else iter(n - 1, f, f(x)) }
  
  /*
  let _ =
  	let msg = string_of_int (iter (6, (fun x -> 2+x)) 0) in
  	print_endline msg
  */
}