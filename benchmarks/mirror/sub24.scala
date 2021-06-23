import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub24 {
  /* problem 1*/
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
          case Empty => { Empty }
          case Node(a, Empty, Empty) => { t }
          case Node(a, Empty, right) => { Node(a, mirror(right), Empty) }
          case Node(a, left, Empty) => { Node(a, Empty, mirror(left)) }
          case Node(a, left, right) => { Node(a, mirror(right), mirror(left)) }
        }
    }
  )
}