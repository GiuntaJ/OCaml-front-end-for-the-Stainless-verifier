import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub22 {
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
          case Node(a, left, right) => {
            
              if (
                a == n
              ) {
                true 
              } else if (
                mem(n, left) || mem(n, right)
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