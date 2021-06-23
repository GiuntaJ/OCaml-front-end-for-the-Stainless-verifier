import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub101 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  val get_head: Crazy2 => Int63 = (
    (c) =>
      {
        c match {
          case NIL => { 0 }
          case ZERO(_c) => { 0 }
          case ONE(_c) => { 1 }
          case MONE(_c) => { -(1) }
        }
    }
  )
  
  val make_crazy2: (Int63, Crazy2) => Crazy2 = {
    case (i, c) =>
      {
        
          if (
            i > 0
          ) {
            ONE(c) 
          } else if (
            i < 0
          ) {
            MONE(c) 
          } else {
            ZERO(c)
          }
    }
  }
  
  val get_tail: Crazy2 => Crazy2 = (
    (c) =>
      {
        c match {
          case NIL => { NIL }
          case ZERO(_c) => { _c }
          case ONE(_c) => { _c }
          case MONE(_c) => { _c }
        }
    }
  )
  
  
  def crazy2add: (Crazy2, Crazy2) => Crazy2 = {
    case (c1, c2) =>
      {
        val _2 = {
          def my_add: (Crazy2, Crazy2, Crazy2) => Crazy2 = {
            case (c1, c2, cr) =>
              {
                
                  if (
                    c1 == NIL && c2 == NIL
                  ) {
                    cr 
                  } else if (
                    c1 == NIL && cr == NIL
                  ) {
                    c2 
                  } else if (
                    c1 == NIL
                  ) {
                    my_add(c2, cr, c1) 
                  } else if (
                    c2 == NIL && cr == NIL
                  ) {
                    c1 
                  } else if (
                    c2 == NIL
                  ) {
                    my_add(c1, cr, c2) 
                  } else if (
                    get_head(c1) + get_head(c2) + get_head(cr) > 1
                  ) {
                    make_crazy2(
                      get_head(c1) + get_head(c2) + get_head(cr) - 2,
                      my_add(get_tail(c1), get_tail(c2), ONE(NIL))) 
                  } else if (
                    get_head(c1) + get_head(c2) + get_head(cr) < -(1)
                  ) {
                    make_crazy2(
                      get_head(c1) + get_head(c2) + get_head(cr) + 2,
                      my_add(get_tail(c1), get_tail(c2), MONE(NIL))) 
                  } else {
                    make_crazy2(
                      get_head(c1) + get_head(c2) + get_head(cr),
                      my_add(get_tail(c1), get_tail(c2), NIL))
                  }
            }
          }
          my_add(c1, c2, NIL)
        }
    }
  }
}
