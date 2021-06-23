import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub44 {
  /* problem 1*/
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  
  val mirror: Btree => Btree = (
    (t) =>
      {
        val _4 = {
          def swap(f) = {
            f match {
              case Empty => { Empty }
              case Node(a, x, y) => { Node(a, swap(y), swap(x)) }
            }
          }
          swap(t)
        }
    }
  )
}