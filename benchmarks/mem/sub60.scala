import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub60 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def mem(n: Int63, tree: Btree): Boolean = {
    tree match {
      case Empty => { false }
      case Node(x, l, r) => { if (n == x) true else mem(n, l) || mem(n, r) }
    }
  }
}