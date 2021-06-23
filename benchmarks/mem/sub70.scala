import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub70 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def mem(a: Int63, t: Btree): Boolean = {
    t match {
      case Empty => { false }
      case Node(q, p, r) => { if (q == a) true else mem(a, p) || mem(a, r) }
    }
  }
}