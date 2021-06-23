import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub123 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  	
  val t1: Btree = Node(1, Empty, Empty)
  val t2: Btree = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
  
  def mem: (Int63, Btree) => Boolean = {
    case (n, tree) =>
      {
        tree match {
          case Empty => { false }
          case Node(a, Empty, Empty) => { a == n }
          case Node(a, b, c) => { if (a == n) true else mem(n, b) || mem(n, c) }
        }
    }
  }
}