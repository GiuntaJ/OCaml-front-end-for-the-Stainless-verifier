import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub266 {
  val iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      {
        val _2 = {
          def r_iter(((n, f, r))) = {
            if (n eq 0) r else r_iter(n - 1, f, ( (x) => { f(r(x)) } ))
          }
          val _3 = {
            def id(x) = { x }
            if (n eq 0) id else r_iter(n, f, id)
          }
        }
    }
  }
}