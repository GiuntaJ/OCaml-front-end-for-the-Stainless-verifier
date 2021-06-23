import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub80 {
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
          case Node(m, a, Empty) => { n == m || mem(n, a) }
          case Node(m, Empty, b) => { n == m || mem(n, b) }
          case Node(m, a, b) => { n == m || mem(n, a) || mem(n, b) }
        }
    }
  }
}