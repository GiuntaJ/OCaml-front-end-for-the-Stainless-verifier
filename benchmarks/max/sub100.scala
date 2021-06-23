import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub100 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { 0 }
          case Cons(h, t) => {
            val _2 = {
              def fun2: (List[Int63], Int63) => Int63 = {
                case (lst1, a) =>
                  {
                    lst1 match {
                      case Nil() => { a }
                      case Cons(hd, tl) => {
                        if (hd > a) fun2(tl, hd) else fun2(tl, a)
                      }
                    }
                }
              }
              fun2(t, h)
            }
          }
        }
    }
  )
    
}