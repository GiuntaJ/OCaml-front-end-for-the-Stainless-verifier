import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub70 {
  val first: List[Int63] => Int63 = (
    (f) =>
      {
        f match {
          case Nil() => { 0 }
          case Cons(hd, tl) => { hd }
        }
    }
  )
  
  val switch: (Int63, List[Int63]) => List[Int63] = {
    case (x, l) =>
      {
        l match {
          case Nil() => { List(x) }
          case Cons(hd, tl) => { x :: tl }
        }
    }
  }
  
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { 0 }
          case Cons(hd, tl) => {
            
              if (
                tl == Nil()
              ) {
                hd 
              } else if (
                hd > first(tl)
              ) {
                max(switch(hd, tl)) 
              } else {
                max(tl)
              }
          }
        }
    }
  )
   
}