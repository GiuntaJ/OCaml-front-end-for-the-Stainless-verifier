import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub58 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
   val t1 = Node(1, Empty, Empty)
   val t2 = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
  
  def mem: (Int63, Btree) => Boolean = {
    case (n, tree) =>
      {
        tree match {
          case Empty => { false }
          case Node(x, left, right) => { n == x || mem(n, left) || mem(n, right)
          }
        }
    }
  }
}