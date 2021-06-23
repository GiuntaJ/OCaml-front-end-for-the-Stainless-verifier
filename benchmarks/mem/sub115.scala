import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub115 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def mem(n: Int63, bt: Btree): Boolean = {
    bt match {
      case Empty => { false }
      case Node(x, b1, b2) => { if (x eq n) true else mem(n, b1) || mem(n, b2) }
    }
  }
}