import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub87 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((f, v))) = {
    val _2 = {
      def deployEnv(env, o) = {
        env match {
          case Cons(a, b) => {
            a match {
              case (x, c, p) => {
                
                  if (
                    o == 0 && c == 0
                  ) {
                    deployEnv(b, o) 
                  } else if (
                    x == "const" && o == 1 && c == 1
                  ) {
                    deployEnv(b, o) 
                  } else if (
                    p == 0
                  ) {
                    CONST(c) :: deployEnv(b, o) 
                  } else if (
                    c == 1 && p == 1
                  ) {
                    VAR(x) :: deployEnv(b, o) 
                  } else if (
                    p == 1
                  ) {
                    TIMES(List(CONST(c), VAR(x))) :: deployEnv(b, o) 
                  } else if (
                    c == 1
                  ) {
                    POWER(x, p) :: deployEnv(b, o) 
                  } else {
                    TIMES(List(CONST(c), POWER(x, p))) :: deployEnv(b, o)
                  }
              }
            }
          }
          case Nil() => { Nil() }
        }
      }
      val _3 = {
        def updateEnv(o, e, env) = {
          env match {
            case Cons(hd, tl) => {
              hd match {
                case (s, c, p) => {
                  e match {
                    case (s2, c2, p2) => {
                      
                        if (
                          o == 0
                        ) {
                          
                            if (
                              s == s2 && p == p2
                            ) {
                              ((s, c + c2, p)) :: tl 
                            } else {
                              hd :: updateEnv(o, e, tl)
                            } 
                        } else if (
                          s == s2
                        ) {
                          ((s, c * c2, p + p2)) :: tl 
                        } else {
                          hd :: updateEnv(o, e, tl)
                        }
                    }
                  }
                }
              }
            }
            case Nil() => { List(e) }
          }
        }
        val _4 = {
          def doDiff(((f, v))) = {
            f match {
              case CONST(_) => { CONST(0) }
              case VAR(x) => { if (x == v) CONST(1) else CONST(0) }
              case POWER(x, p) => {
                
                  if (
                    p == 0
                  ) {
                    CONST(0) 
                  } else if (
                    x == v
                  ) {
                    TIMES(List(CONST(p), POWER(x, p - 1))) 
                  } else {
                    CONST(0)
                  }
              }
              case TIMES(x) => {
                x match {
                  case Cons(a, Nil()) => { doDiff(a, v) }
                  case Cons(a, b) => {
                    val _7 = {
                      val da = doDiff(a, v)
                      val _8 = {
                        val db = doDiff(TIMES(b), v)
                        (a, db, b, da) match {
                          case (CONST(p), CONST(q), Cons(CONST(r), Nil()),
                          CONST(s)) => {
                            CONST(p * q + r * s)
                          }
                          case (CONST(p), CONST(q), _, _) => {
                            
                              if (
                                da == CONST(0) || b == List(CONST(0))
                              ) {
                                CONST(p * q) 
                              } else {
                                SUM(List(CONST(p * q), TIMES(da :: b)))
                              }
                          }
                          case (_, _, Cons(CONST(r), Nil()), CONST(s)) => {
                            
                              if (
                                a == CONST(0) || db == CONST(0)
                              ) {
                                CONST(r * s) 
                              } else {
                                SUM(List(TIMES(List(a, db)), CONST(r * s)))
                              }
                          }
                          case _ => {
                            
                              if (
                                a == CONST(0) || db == CONST(0)
                              ) {
                                TIMES(da :: b) 
                              } else if (
                                b == List(CONST(0)) || da == CONST(0)
                              ) {
                                TIMES(List(a, db)) 
                              } else {
                                SUM(List(TIMES(List(a, db)), TIMES(da :: b)))
                              }
                          }
                        }
                      }
                    }
                  }
                  case Nil() => { CONST(0) }
                }
              }
              case SUM(x) => { SUM(x.map(( (x) => { doDiff(x, v) } ))) }
            }
          }
          val _9 = {
            def prettyAE(f, env, o) = {
              f match {
                case SUM(x) => {
                  x match {
                    case Cons(CONST(a), z) => {
                      prettyAE(SUM(z), updateEnv(0, "const", a, 0, env), 0)
                    }
                    case Cons(VAR(a), z) => {
                      prettyAE(SUM(z), updateEnv(0, a, 1, 1, env), 0)
                    }
                    case Cons(POWER(a, b), z) => {
                      prettyAE(SUM(z), updateEnv(0, a, 1, b, env), 0)
                    }
                    case Cons(SUM(y), z) => { prettyAE(SUM(y ++(z)), env, 0) }
                    case Cons(TIMES(y), z) => {
                      val _15 = {
                        val l = prettyAE(TIMES(y), Nil(), 1)
                        l match {
                          case Cons(h, t) => {
                            
                              if (
                                t == Nil()
                              ) {
                                l ++(prettyAE(SUM(z), env, 0)) 
                              } else {
                                List(TIMES(l)) ++(prettyAE(SUM(z), env, 0))
                              }
                          }
                          case Nil() => { Nil() }
                        }
                      }
                    }
                    case Nil() => { deployEnv(env, 0) }
                  }
                }
                case TIMES(x) => {
                  x match {
                    case Cons(CONST(a), z) => {
                      prettyAE(TIMES(z), updateEnv(1, "const", a, 0, env), 1)
                    }
                    case Cons(VAR(a), z) => {
                      prettyAE(TIMES(z), updateEnv(1, a, 1, 1, env), 1)
                    }
                    case Cons(POWER(a, b), z) => {
                      prettyAE(TIMES(z), updateEnv(1, a, 1, b, env), 1)
                    }
                    case Cons(SUM(y), z) => {
                      val _12 = {
                        val l = prettyAE(SUM(y), Nil(), 0)
                        l match {
                          case Cons(h, t) => {
                            
                              if (
                                t == Nil()
                              ) {
                                l ++(prettyAE(TIMES(z), env, 1)) 
                              } else {
                                List(SUM(l)) ++(prettyAE(TIMES(z), env, 1))
                              }
                          }
                          case Nil() => { Nil() }
                        }
                      }
                    }
                    case Cons(TIMES(y), z) => { prettyAE(TIMES(y ++(z)), env, 1)
                    }
                    case Nil() => { deployEnv(env, 1) }
                  }
                }
              }
            }
            val _16 = {
              val res = doDiff(f, v)
              res match {
                case SUM(_) => { SUM(prettyAE(res, Nil(), 0)) }
                case TIMES(_) => { TIMES(prettyAE(res, Nil(), 1)) }
                case _ => { res }
              }
            }
          }
        }
      }
    }
  }
}