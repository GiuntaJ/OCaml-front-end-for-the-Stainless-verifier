import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mirror_sub94 {
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
          def f(t) = {
            t match {
              case Node(n, a, b) => {
                
                  if (
                    a == Empty && b == Empty
                  ) {
                    Node(n, b, a) 
                  } else {
                    Node(n, f(b), f(a))
                  }
              }
              case Empty => { Empty }
            }
          }
          f(t)
        }
    }
  ) 
}