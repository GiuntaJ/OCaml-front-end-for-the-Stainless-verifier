import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub71 {
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
          case Node(i, a, b) => {
            val _2 = {
              val v1 = mirror(a)
              val _3 = {
                val v2 = mirror(b)
                Node(i, v2, v1)
              }
            }
          }
        }
    }
  )
}
