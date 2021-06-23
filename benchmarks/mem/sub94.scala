import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_mem_sub94 {
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
        val _2 = {
          def leaves = (
            x =>
              x match {
                case Empty => { Nil() }
                case Node(c, Empty, Empty) => { List(c) }
                case Node(_, l, r) => { leaves(l) ++ leaves(r) }
              }
          )
          val _3 = {
            def search(a) = {
              (
                x =>
                  x match {
                    case Nil() => { false }
                    case Cons(hd, tl) => { if (hd == a) true else search(a, tl)
                    }
                  }
              )
            }
            search(n, leaves(tree))
          }
        }
    }
  }
}