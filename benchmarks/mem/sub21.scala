import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub21 {
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
          case Empty => { false }
          case Node(m, Empty, Empty) => { n == m }
          case Node(m, Empty, b1) => { mem(n, b1) || n == m }
          case Node(m, b1, Empty) => { mem(n, b1) || n == m }
          case Node(m, b1, b2) => { mem(n, b1) || mem(n, b2) || n == m }
        }
    }
  }
}