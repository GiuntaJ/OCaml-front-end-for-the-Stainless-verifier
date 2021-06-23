import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub31 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  /*let t1 = Node (1, Empty, Empty);;*/
  /*let t2 = Node (1, Node(2,Empty,Empty), Node(3,Empty,Empty));;*/
  
  def mem: (Int63, Btree) => Boolean = {
    case (n, tree) =>
      {
        tree match {
          case Empty => { false }
          case Node(_, Empty, _) => { if (n == 1) true else false }
          case Node(_, left, right) => { mem(n / 2, left) }
        }
    }
  }
}