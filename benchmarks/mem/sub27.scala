import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub27 {
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
      case Node(c, left, right) => {
        if (n == c) true else mem(n, left) || mem(n, right)
      }
    }
  }
}