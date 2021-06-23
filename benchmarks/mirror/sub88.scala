import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub88 {
  /*problem 1*/
    sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
    def mirror: Btree => Btree = (
    (t) =>
      {
        t match {
          case Node(n, Empty, Empty) => { Node(n, Empty, Empty) }
          case Node(n, x, y) => { Node(n, mirror(y), mirror(x)) }
          case Empty => { Empty }
        }
    }
  )
}