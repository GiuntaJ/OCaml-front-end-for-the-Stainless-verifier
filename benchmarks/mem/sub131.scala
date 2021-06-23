import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub131 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  def mem(n: Int63, btree: Btree): Boolean = {
    btree match {
      case Empty => { false }
      case Node(element, left_btree, right_btree) => {
        if (element eq n) true else mem(n, left_btree) || mem(n, right_btree)
      }
    }
  }
}