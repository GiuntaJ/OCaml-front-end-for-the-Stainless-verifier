import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub236 {
  def iter: ((Int63, (X => X)), X) => X = (
    (p) =>
      {
        p match {
          case (a, b) => {
            
              if (
                a eq 0
              ) {
                ( (x) => { x } ) 
              } else {
                ( (x) => { b(iter(a - 1, b, x)) } )
              }
          }
        }
    }
  )
}