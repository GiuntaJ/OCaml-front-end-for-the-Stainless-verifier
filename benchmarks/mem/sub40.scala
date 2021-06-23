import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub40 {
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
          case Node(m, btreex, btreey) => {
            
              if (
                m == n
              ) {
                true 
              } else if (
                btreex == Empty
              ) {
                mem(n, btreey) 
              } else if (
                btreey == Empty
              ) {
                mem(n, btreex) 
              } else {
                mem(n, btreex) || mem(n, btreey)
              }
          }
        }
    }
  }
}