import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub75 {
  /* problem 1 */
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
          def f(t) = {
            t match {
              case Empty => { Empty }
              case Node(n, tree1, tree2) => { Node(n, f(tree2), f(tree1)) }
            }
          }
          f(t)
        }
    }
  )
}