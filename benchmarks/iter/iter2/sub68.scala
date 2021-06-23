import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub68 {
  /*Problem 3*/
  def comp(f, g, x) = { f(g(x)) }
  
  def loop: ((Int63, (Int63 => Int63), (Int63 => Int63)), Int63) => Int63 = {
    case (n, f, g) => { if (n == 1) g else loop(n - 1, f, comp(f, g)) }
  }
  
  def iden_func[A](n: A): A = { n }
  
  def error[A](n: A): Int63 = { -(1) } 
  
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n < 0
          ) {
            error 
          } else if (
            n == 0
          ) {
            iden_func 
          } else {
            loop(n, f, f)
          }
    }
  }
}