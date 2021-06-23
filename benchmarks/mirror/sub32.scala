import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub32 {
  sealed abstract class Btree {}
  case object Empty extends Btree {}
  case class Node(
    param0: Int63, 
    param1: Btree, 
    param2: Btree
  ) extends Btree {}
  def mirror(t: Btree): Btree = {
    t match {
      case Node(x1, y1, z1) => { Node(x1, mirror(z1), mirror(y1)) }
      case Empty => { Empty }
    }
  }
}