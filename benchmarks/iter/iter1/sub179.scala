import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub179 {
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      {
        
          if (
            n > 0
          ) {
            val _3 = {
              def composition(x) = { iter(n - 1, f, f(x)) }
              composition
            } 
          } else {
            val _4 = {
              def iden(x) = { x }
              iden
            }
          }
    }
  }
  /*let func x = (x*x)-1
  let ans = iter(3,func) 2
  let _= print_endline(string_of_int ans)*/
}