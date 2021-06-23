import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub81 {
  /*problem 1*/
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
          def mirro(t) = {
            t match {
              case Empty => { Empty }
              case Node(x, y, z) => { Node(x, mirro(z), mirro(y)) }
            }
          }
          mirro(t)
        }
    }
  )
}