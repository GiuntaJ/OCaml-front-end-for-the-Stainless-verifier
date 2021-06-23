import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub83 {
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
          case Empty => { Empty }
          case Node(root, Empty, Empty) => { Node(root, Empty, Empty) }
          case Node(root, left, Empty) => { Node(root, Empty, mirror(left)) }
          case Node(root, Empty, right) => { Node(root, mirror(right), Empty) }
          case Node(root, left, right) => {
            Node(root, mirror(right), mirror(left))
          }
        }
    }
  )
}