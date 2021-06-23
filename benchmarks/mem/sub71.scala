import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub71 {
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
          case Node(p, q, r) => {
            
              if (
                p == n
              ) {
                true 
              } else if (
                mem(n, q)
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
}