import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub40 {
  /* Name: Yoon Jae Nam (2012-81338)
     Organization: Seoul National University
     Class: Programming Language (4190.310)
     Assignment: 2
     Problem: 2: Mathemadiga */
  
  /* 1. Provided declarations / definitions */
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  /*)
  
  /* 2. My code */
  val zero: Ae = CONST(0)
  val one: Ae = CONST(1)
  
  /* Helper function that simplifies an ae */
  def simplifyAe: Ae => Ae = (
    (exp) =>
      {
        exp match {
          case POWER(var0, e) => {
            e match {
              case 0 => { one }
              case 1 => { VAR(var0) }
              case _ => { exp }
            }
          }
          case TIMES(ae_list) => {
            ae_list match {
              case Nil() => { one }
              case Cons(h, Nil()) => { simplifyAe(h) }
              case Cons(h, t) => {
                h match {
                  case CONST(0) => { zero }
                  case CONST(1) => { simplifyAe(TIMES(t)) }
                  case _ => {
                    val _6 = {
                      val t_simp = simplifyAe(TIMES(t))
                      val _7 = {
                        val h_simp = simplifyAe(h)
                        (h_simp, t_simp) match {
                          case (CONST(0), _) => { zero }
                          case (_, CONST(0)) => { zero }
                          case (CONST(1), _) => { t_simp }
                          case (_, CONST(1)) => { h_simp }
                          case (_, _) => { TIMES(List(h_simp, t_simp)) }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          case SUM(ae_list) => {
            ae_list match {
              case Nil() => { zero }
              case Cons(h, Nil()) => { simplifyAe(h) }
              case Cons(h, t) => {
                h match {
                  case CONST(0) => { simplifyAe(SUM(t)) }
                  case _ => {
                    val _2 = {
                      val t_simp = simplifyAe(SUM(t))
                      val _3 = {
                        val h_simp = simplifyAe(h)
                        (h_simp, t_simp) match {
                          case (CONST(0), _) => { t_simp }
                          case (_, CONST(0)) => { h_simp }
                          case (_, _) => { SUM(List(h_simp, t_simp)) }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          case _ => { exp }
        }
    }
  )
  
  def removeIndex: (List[Ae], Int63) => List[Ae] = {
    case (l, index_to_remove) =>
      {
        
          if (
            index_to_remove == 0
          ) {
            l.tail 
          } else {
            l.head :: removeIndex(l.tail, index_to_remove - 1)
          }
    }
  }
  
  val getPartialProduct: (List[Ae], List[Ae], Int63) => Ae = {
    case (l_orig, l_diff, index) =>
      {
        val _10 = {
          val elem_diff: Ae = l_diff.apply(index)
          val _11 = {
            val l_orig_without_index: List[Ae] = removeIndex(l_orig, index)
            val _12 = {
              val product: List[Ae] = elem_diff :: l_orig_without_index
              TIMES(product)
            }
          }
        }
    }
  }
  
  def getAllPartialProductsHelper: (List[Ae], List[Ae], Int63) => List[Ae] = {
    case (l_orig, l_diff, upper_index) =>
      {
        
          if (
            upper_index < 0
          ) {
            Nil() 
          } else {
            val _16 = {
              val current_partial_product = getPartialProduct(l_orig, l_diff, upper_index)
              val _17 = {
                val prev_partial_products = getAllPartialProductsHelper(l_orig, l_diff, upper_index - 1)
                current_partial_product :: prev_partial_products
              }
            }
          }
    }
  }			
  
  val getAllPartialProducts: (List[Ae], List[Ae]) => List[Ae] = {
    case (l_orig, l_diff) =>
      {
        val _20 = {
          val size = l_orig.length
          val _21 = {
            val result = getAllPartialProductsHelper(l_orig, l_diff, size - 1)
            result.reverse
          }
        }
    }
  }
  
  def diff: (Ae, String) => Ae = {
    case (expr, var_diff) =>
      {
        expr match {
          case CONST(i) => { zero }
          case VAR(var0) => { if (var0 == var_diff) one else zero }
          case POWER(var0, e) => {
            
              if (
                var0 != var_diff
              ) {
                zero 
              } else {
                e match {
                  case 0 => { zero }
                  case 1 => { one }
                  case _ => {
                    simplifyAe(TIMES(List(CONST(e), POWER(var0, e - 1))))
                  }
                }
              }
          }
          case TIMES(ae_list_orig) => {
            val _29 = {
              val ae_list_diff = ae_list_orig.map(( (ae_elem) => { diff(ae_elem, var_diff) } ))
              val _30 = {
                val to_sum = getAllPartialProducts(ae_list_orig, ae_list_diff)
                simplifyAe(SUM(to_sum))
              }
            }
          }
          case SUM(ae_list) => {
            ae_list match {
              case Nil() => { zero }
              case Cons(h, Nil()) => { diff(h, var_diff) }
              case Cons(h, t) => {
                val _24 = {
                  val first_diff: Ae = diff(h, var_diff)
                  val _25 = {
                    val rest_diff: Ae = diff(SUM(t), var_diff)
                    val _26 = {
                      val combined_diff = List(first_diff, rest_diff)
                      simplifyAe(SUM(combined_diff))
                    }
                  }
                }
              }
            }
          }
        }
    }
  }
  
  /* 3. Test code */
  /*
  /* ax^2 + bx + c */
  let testExpr1 = SUM([
  	TIMES([
  		VAR("a");
  		POWER("x",2)
  	]); /* ax^2 */
  	TIMES([
  		VAR("b");
  		VAR("x")
  	]); /* bx */
  	VAR("c") /* c */
  ])
  /*)
  /* x^5 + 3 + bxc^2 */
  let testExpr2 = SUM([
  	SUM([CONST(3);POWER("x",5)]); /* x^5 + 3*/
  	TIMES([
  		VAR("b");
  		VAR("x");
  		POWER("c",2)
  	]) /* bxc^2 */
  ])
  	
  let rec printAe : ae -> unit = fun expr ->
  	match expr with
  	| CONST(n) -> Printf.printf "CONST(%d)" n
  	| VAR(str) -> Printf.printf "VAR(%s)" str
  	| POWER(str,n) -> Printf.printf "POWER(%s,%d)" str n
  	| TIMES(ae_list) ->
  		let sz = List.length ae_list in
  			(print_string "TIMES([";
  			for i = 0 to sz - 1 do
  				printAe(List.nth ae_list i);
  				if i <> (sz - 1) then print_string ";"
  			done;
  			print_string "])";
  			)
  	| SUM(ae_list) ->
  		let sz = List.length ae_list in
  			(print_string "SUM([";
  			for i = 0 to sz - 1 do
  				printAe(List.nth ae_list i);
  				if i <> (sz - 1) then print_string ";"
  			done;
  			print_string "])";
  			)
  /*)
  let testRunner = fun (expr,var, msg) ->
  	print_endline "====================================";
  	print_endline("                           " ^ msg);
  	print_endline "Expression:";
  	printAe(expr);
  	print_endline "
  ----------";
  	print_endline ("Diff by: " ^ var);
  	print_endline "----------";
  	let diff_result = diff(expr,var) in
  		(print_endline "Result:";
  		printAe(diff_result);
  		print_newline())
  /*)
  let test =
  	let expr = testExpr1 in
  	let var = "x" in
  	let msg = "Good" in
  	testRunner(expr,var, msg)
  /*)
  let test =
  	let expr = VAR("x") in
  	let var = "x" in
  	let msg = "Good" in
  	testRunner(expr,var, msg)
  /*)
  let test =
  	let expr = CONST(5) in
  	let var = "x" in
  	let msg = "Good" in
  	testRunner(expr,var, msg)
  /*)
  let test =
  	let expr = POWER("x",2) in
  	let var = "x" in
  	let msg = "Good" in
  	testRunner(expr,var, msg)
  /*)
  let test =
  	let expr = POWER("x",3) in
  	let var = "x" in
  	let msg = "Good" in
  	testRunner(expr,var, msg)
  /*)
  let test =
  	let expr = TIMES([CONST(3);VAR("x");VAR("a")]) in
  	let var = "x" in
  	let msg = "Good" in
  	testRunner(expr,var, msg)
  /*)
  let test =
  	let expr = testExpr2 in
  	let var = "x" in
  	let msg = "Good" in
  	testRunner(expr,var, msg)
  /*)
  let test =
  	let expr = testExpr2 in
  	let var = "d" in
  	let msg = "Good" in
  	testRunner(expr,var, msg)
  /*)
  let test =
  	let expr = testExpr2 in
  	let var = "b" in
  	let msg = "Good" in
  	testRunner(expr,var, msg)
  /*)
  let test =
  	let expr = VAR("x") in
  	let var = "k" in
  	let msg = "Good" in
  	testRunner(expr,var, msg)
  /*)
  let test =
  	let expr = testExpr1 in
  	let var = "a" in
  	let msg = "Good" in
  	testRunner(expr,var, msg)
  let test =
  	let expr = testExpr1 in
  	let var = "c" in
  	let msg = "Good" in
  	testRunner(expr,var, msg)
  /*)
  */
}