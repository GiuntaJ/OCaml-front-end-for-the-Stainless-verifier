import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub193 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        val _4 = {
          val listHead: List[Int63] => Int63 = (
            (lst) =>
              {
                lst match {
                  case Cons(n, _) => { n }
                  case Nil() => { -(1) }
                }
            }
          )
          val _5 = {
            def findMax: (List[Int63], Int63) => Int63 = {
              case (lst, max) =>
                {
                  lst match {
                    case Cons(n, t) => {
                      if (max < n) findMax(t, n) else findMax(t, max)
                    }
                    case Nil() => { max }
                  }
              }
            }
            findMax(lst, listHead(lst))
          }
        }
    }
  )
  	/*
  	match max with
  	List.fold_left (fun a b -> if a > b then a else b) max lst
  	*/
   
}