import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_crazy2add_sub229 {
  sealed abstract class Crazy2 {}
  case object NIL extends Crazy2 {}
  case class ZERO(param0: Crazy2) extends Crazy2 {}
  case class ONE(param0: Crazy2) extends Crazy2 {}
  case class MONE(param0: Crazy2) extends Crazy2 {}
  
  def power(num: Int63, expo: Int63): Int63 = {
    expo match {
      case 0 => { 1 }
      case 1 => { num }
      case any => {
        val _2 = {
          val x = power(num, any / 2)
          x * x * (if (any % 2 == 0) 1 else num)
        }
      }
    }
  }
  
  def cval(((crz: Crazy2, i: Int63))): Int63 = {
    crz match {
      case NIL => { 0 }
      case ZERO(x) => { cval(x, i + 1) }
      case ONE(x) => { 1 * power(2, i) + cval(x, i + 1) }
      case MONE(x) => { -(1) * power(2, i) + cval(x, i + 1) }
    }
  }
  
  def crazy2val(crz: Crazy2): Int63 = { cval(crz, 0) }
  
  def makelist(((x: Int63, l: List[Int63]))): List[Int63] = {
    l match {
      case Nil() => { List(x) }
      case Cons(hd, tl) => { x :: l }
    }
  } 
  val flist: List[Int63] = makelist(1, Nil())
  def slist(((x: Int63, l: List[Int63]))): List[Int63] = {
    if (x > 0) slist(x - 1, makelist(0, l)) else l
  }  
  def dlist(((x: List[Int63], y: List[Int63]))): List[List[Int63]] = {
    List(x, y)
  } 
  def mlist(((x: List[Int63], y: List[List[Int63]]))): List[List[Int63]] = {
    x :: y
  }
  
  
  def finder(((x: Int63, i: Int63))): Int63 = {
    if (power(2, i) > x) i - 1 else finder(x, i + 1)
  } 
  def remain(x: Int63): Int63 = { x - power(2, finder(x, 0)) }
  
  
  def fl(((x: Int63, l: List[List[Int63]]))): List[List[Int63]] = {
    if (x > 0) fl(remain(x), mlist(slist(finder(x, 0), flist), l)) else l
  }
  def finalist(x: Int63): List[List[Int63]] = { fl(x, List(Nil(), Nil())) }
  
  def ltoint(l: List[Int63]): Int63 = {
    List.fold_right(
      {
        case (x, acc) => { x + acc * 10 }
      },
      l, 0)
  }
  
  def suml(((x: List[List[Int63]], sum: Int63))): Int63 = {
    x match {
      case Nil() => { sum }
      case Cons(hd, tl) => { suml(tl, sum + ltoint(hd)) }
    }
  }
  def sumlist(x: Int63): Int63 = { suml(finalist(x), 0) }
  
  def digits(((n: Int63, l: List[Int63]))): List[Int63] = {
    if (n > 0) digits(n / 10, n % 10 :: l) else l
  }
  def rl(((l: List[Int63], rel: List[Int63]))): List[Int63] = {
    l match {
      case Nil() => { rel }
      case Cons(hd, tl) => { rl(tl, hd :: rel) }
    }
  } 
  def relist(x: Int63): List[Int63] = { rl(digits(x, Nil()), Nil()) }
  
  def abs(x: Int63): Int63 = { if (x < 0) -(x) else x }		
  
  def replace[A](l: List[A], pos: Int63, a: A): List[A] = {
    List.mapi(
      {
        case (i, x) => { if (i == pos) a else x }
      },
      l)
  }
  def fillneg(((l: List[Int63], i: Int63))): List[Int63] = {
    
      if (
        i < l.length
      ) {
        l.apply(i) match {
          case 0 => { fillneg(l, i + 1) }
          case 1 => { fillneg(replace(l, i, -(1)), i + 1) }
        } 
      } else {
        l
      }
  }
  
  def turNeg(x: List[Int63]): List[Int63] = { fillneg(x, 0) }
  
  def translate(l: List[Int63]): Crazy2 = {
    l match {
      case Nil() => { NIL }
      case Cons(hd, tl) => {
        hd match {
          case 0 => { ZERO(translate(tl)) }
          case 1 => { ONE(translate(tl)) }
          case -1 => { MONE(translate(tl)) }
        }
      }
    }
  } 
  def translate2(x: Int63): Crazy2 = {
    
      if (
        x < 0
      ) {
        translate(turNeg(relist(sumlist(abs(x))))) 
      } else {
        translate(relist(sumlist(abs(x))))
      }
  }	
  
  def crazy2add(((x: Crazy2, y: Crazy2))): Crazy2 = {
    translate2(crazy2val(x) + crazy2val(y))
  }
}