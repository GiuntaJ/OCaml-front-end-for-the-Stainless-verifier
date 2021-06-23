import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub155 {
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { 0 }
          case Cons(hd, tl) => {
            val _2 = {
              def compare(a, b) = { if (a > b) a else b }
              val _3 = {
                def fold(compare, m, n) = {
                  m match {
                    case Nil() => { n }
                    case Cons(hd, tl) => { compare(hd, fold(compare, tl, n)) }
                  }
                }
                fold(compare, tl, hd)
              }
            }
          }
        }
    }
  )
  
   
}