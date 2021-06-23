import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub111 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def mem: (Int63, Btree) => Boolean = {
    case (n, tree) =>
      {
        tree match {
          case Node(num, t1, t2) => {
            
              if (
                n == num
              ) {
                true 
              } else if (
                mem(n, t1) || mem(n, t2)
              ) {
                true 
              } else {
                false
              }
          }
          case Empty => { false }
        }
    }
  }
}