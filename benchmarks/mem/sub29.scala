import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub29 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  def mem(n: Int63, t: Btree): Boolean = {
    t match {
      case Empty => { false }
      case Node(p, l, r) => {
        
          if (
            p eq n
          ) {
            true 
          } else if (
            mem(n, l)
          ) {
            true 
          } else if (
            mem(n, r)
          ) {
            true 
          } else {
            false
          }
      }
    }
  }
}