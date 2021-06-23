import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub96 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def composite(a, b, x) = { a(b(x)) }
          val _3 = {
            def repeat(c, n) = {
              if (n <= 0) ( (x) => { x } ) else composite(c, repeat(c, n - 1))
            }
            n match {
              case 0 => { ( (x) => { x } ) }
              case _ => { repeat(f, n) }
            }
          }
        }
    }
  } 
      
  iter(0, ( (x) => { x + 2 } ), 0)
  iter(3, ( (x) => { x * 2 } ), 2)
  iter(5, ( (x) => { x + 2 } ), 0)
}