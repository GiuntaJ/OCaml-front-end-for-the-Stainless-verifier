#load "pa_extfun.cmo";
#load "pa_extprint.cmo";
#load "pa_pprintf.cmo";
#load "q_MLast.cmo";

open Mlsyntax.Original;
open Pcaml;
open Pretty;
open Prtools;
open Versdep;

(* Formatting *)

value op_after elem pc (x, op) = pprintf pc "%p%s" elem x op;
value double_line_return_before elem pc x = pprintf pc "@[<b>@ %p@]" elem x;
value comma_after elem pc x = pprintf pc "%p, " elem x;

value operator pc left right sh (loc, op) x y =
  let op = if op = "" then "" else " " ^ op in
    pprintf pc "%p%s@;%p" left x op right y
;

value left_operator pc loc sh unfold next x =
  let xl =
    loop [] x "" where rec loop xl x op =
      match unfold x with
      [ Some (x1, op1, x2) -> loop [(x2, op) :: xl] x1 op1
      | None -> [(x, op) :: xl] ]
  in
  match xl with
  [ [(x, _)] -> next pc x
  | _ ->
      horiz_vertic (fun () -> hlist (op_after next) pc xl)
        (fun () -> plist next sh pc xl) ]
;

value right_operator pc loc sh unfold next x =
  let xl =
    loop [] x where rec loop xl x =
      match unfold x with
      [ Some (x1, op, x2) -> loop [(x1, op) :: xl] x2
      | None -> List.rev [(x, "") :: xl] ]
  in
  match xl with
  [ [(x, _)] -> next pc x
  | _ ->
      horiz_vertic (fun () -> hlist (op_after next) pc xl)
        (fun () -> plist next sh pc xl) ]
;

(* Basic expressions and patterns *)

value ocaml_char =
  fun
  [ "'" -> "\\'"
  | "\\" -> "\\\\"
  | c -> c ]
;

value print_int63 pc ocaml_int = 
  (* Scala Int.MaxValue is equal to 2147483647 *)
  if int_of_string ocaml_int > 2147483647 then 
    pprintf pc "BitVectors.fromLong(%sL).narrow[Int63]" ocaml_int
  else
    pprintf pc "%s" ocaml_int
;

value known_lids = ref [];

value lids pc lid =
  let new_lid = List.assoc_opt lid known_lids.val 
  in
  match new_lid with 
  [ Some new_lid -> pprintf pc "%s" new_lid
  | None -> 
    let has_forbidden_symbol =
      String.contains lid '\'' || 
      List.mem lid 
        ["abstract"; "case"; "catch"; "class"; "def"; "do"; 
         "else"; "extends"; "final"; "finally"; "for"; 
         "forSome"; "if"; "implicit"; "import"; "lazy"; 
         "match"; "new"; "null"; "object"; "override"; 
         "package"; "private"; "protected"; "return"; 
         "sealed"; "super"; "this"; "throw"; "trait"; "try";
         "type"; "val"; "var"; "while"; "with"; "yield"]
    in 
      if has_forbidden_symbol then 
        let new_lid = 
          ref ((String.map (fun c -> if c = '\'' then '_' else c) lid) ^ "0")
        in
        let i = ref 1 in
        let has_duplicate = 
          ref (List.mem_assoc new_lid.val known_lids.val)
        in
        do{
          while has_duplicate.val = True do{ 
            new_lid.val := new_lid.val ^ string_of_int i.val;
            i.val := i.val + 1;
            has_duplicate.val := (List.mem_assoc new_lid.val known_lids.val)
          };
          known_lids.val := 
            List.cons 
              (lid, new_lid.val) 
              (known_lids.val);
          pprintf pc "%s" new_lid.val}
      else 
        do{
          known_lids.val := 
            List.cons 
              (lid, lid) 
              (known_lids.val);
          pprintf pc "%s" lid}
  ]
;

value uids pc uid =
  let x =
    match uid with
    [ "True" -> "true"
    | "False" -> "false"
    | "[]" -> "Nil()"
    | "()" -> "()"
    | "None" -> "None()"
    | _ -> uid ]
  in
  pprintf pc "%s" x
;

(* Printer for each node *)

do {
  Eprinter.clear pr_expr;
  Eprinter.clear pr_patt; 
  Eprinter.clear pr_ctyp;
  Eprinter.clear pr_str_item;
};

value expr = Eprinter.apply pr_expr;
value patt = Eprinter.apply pr_patt;
value ctyp = Eprinter.apply pr_ctyp;
value str_item = Eprinter.apply pr_str_item;
value longident = Eprinter.apply pr_longident;
value expr_fun_args ge = Extfun.apply pr_expr_fun_args.val ge;

value simple_patt = Eprinter.apply_level pr_patt "simple";
value expr1 = Eprinter.apply_level pr_expr "expr1";

(* Checks on expressions and patterns *)

value is_infix = do {
  let infixes = Hashtbl.create 73 in
  List.iter (fun s -> Hashtbl.add infixes s True)
    ["!="; "&&"; "*"; "**"; "*."; "+"; "+."; "-"; "-."; "/"; "/."; "<"; "<=";
     "<>"; "="; "=="; ">"; ">="; "@"; "^"; "asr"; "land"; "lor"; "lsl"; "lsr";
     "lxor"; "mod"; "or"; "||"; "~-"; "~-."];
  fun s -> try Hashtbl.find infixes s with [ Not_found -> False ]
};

value is_unary =
  fun
  [ "-" | "-." -> True
  | _ -> False ]
;

value rec is_irrefut_patt =
  fun
  [
    <:patt< $p$ [@ $_attribute:_$ ] >> -> is_irrefut_patt p
  |  <:patt< $lid:_$ >> -> True
  | <:patt< $uid:"()"$ >> -> True
  | <:patt< _ >> -> True
  | <:patt< $longid:_$ . $y$ >> -> is_irrefut_patt y
  | <:patt< ($x$ as $y$) >> -> is_irrefut_patt x && is_irrefut_patt y
  | <:patt< { $list:fpl$ } >> ->
      List.for_all (fun (_, p) -> is_irrefut_patt p) fpl
  | <:patt< ($p$ : $_$) >> -> is_irrefut_patt p
  | <:patt< ($list:pl$) >> -> List.for_all is_irrefut_patt pl
  | <:patt< (type $lid:_$) >> -> True
  | <:patt< (module $uidopt:_$ : $_$) >> -> True
  | <:patt< (module $uidopt:_$) >> -> True
  | <:patt< ~{$list:_$} >> -> True
  | <:patt< ?{$_$ $opt:_$} >> -> True
  | <:patt< [% $_extension:_$ ] >> -> True
  | _ -> False ]
;


(* Arguments of a function *)

pr_expr_fun_args.val :=
  extfun Extfun.empty with
  [ <:expr< fun $p$ -> $e$ >> as z ->
      if is_irrefut_patt p then
        let (pl, e) = expr_fun_args e in
        ([p :: pl], e)
      else ([], z)
  | z -> ([], z) ]
;

(* Sequence of statements *)

value flatten_sequence e =
  let rec get_sequence =
    fun
    [ <:expr< do { $list:el$ } >> -> Some el
    | _ -> None ]
  in
  match get_sequence e with
  [ Some el ->
      let rec list_of_sequence =
        fun
        [ [e :: el] ->
            match get_sequence e with
            [ Some el1 -> list_of_sequence (el1 @ el)
            | None -> [e :: list_of_sequence el] ]
        | [] -> [] ]
      in
      Some (list_of_sequence el)
  | None -> None ]
;

(* Pattern matching *)

value match_case pc (match_pattern, pattern_guard, match_expr) = 
  pprintf pc "case %p%p => {@;%p@ }" 
    patt match_pattern
    (fun pc ->
      fun
      [ <:vala< Some e >> ->
        pprintf pc "@[ if@;%p@]" expr e
      | _ ->
        pprintf pc "" ]) pattern_guard
    expr match_expr
;

value pattern_matching pc match_expr match_case_list = 
  pprintf pc "@[<b>%p match {@;%p@ }@]" 
    expr match_expr
    (vlist match_case) match_case_list
;

(* Type declarations *)

value known_records = ref [];

value get_known_record_option pc labels = 
  let params_length = List.length labels in
    List.find_opt 
      (fun ((known_params_length, knowns_labels), _) -> 
        known_params_length = params_length && 
        (List.for_all (fun param -> List.mem param knowns_labels) labels)) 
      known_records.val 
;

value print_case_class pc
  sealed_string
  case_class_id 
  td_params_string 
  params 
  param_to_string
  extends =  
  horiz_vertic 
    (fun () -> 
      pprintf pc 
        "@[<a>%scase class %s%s(@[@;<0 2>%p@]@;<0 0>) %s {}@]"
        sealed_string 
        case_class_id
        td_params_string
        (hlistl (comma_after param_to_string) param_to_string) params
        extends)
    (fun () -> 
      pprintf pc 
        "@[<a>%scase class %s%s(@[@;<0 2>%p@]@;<0 0>) %s {}@]"
        sealed_string 
        case_class_id
        td_params_string
        (vlistl (comma_after param_to_string) param_to_string) params
        extends)
;

value print_constructor pc sealed extends type_def_params can_be_object constructor = 
  match constructor with
  [ <:constructor:< $_uid:cons_id$ of $_list:cons_params$ $_rto:rto$ $_algattrs:alg_attrs$ >> ->       
    let cons_id = Pcaml.unvala cons_id in
    let cons_params = Pcaml.unvala cons_params in
    if (can_be_object && List.length cons_params = 0) then 
      pprintf pc "@[<a>case object %p %s {}@]"
        uids cons_id
        extends
    else
      let cons_params = 
        List.mapi (fun index param -> (index, param)) cons_params 
      in
      let ctyp_apply = Eprinter.apply_level pr_ctyp "apply" in
      let param_to_string pc (index, param) = 
        pprintf pc "param%d: %p"
          index 
          ctyp_apply param
      in
        print_case_class pc
          sealed 
          (uids pc cons_id) 
          type_def_params 
          cons_params 
          param_to_string 
          extends ]
  ;

value type_decl pc td = (* TODO isDecl & private & variants & constraints ?*)
  let ((_, td_id), td_params, td_def, td_constraints) =
    (Pcaml.unvala td.MLast.tdNam, Pcaml.unvala td.MLast.tdPrm, td.MLast.tdDef, td.MLast.tdCon)
  in
  let td_id = (String.capitalize_ascii (Pcaml.unvala td_id)) 
  in
  let td_params = 
    List.map 
      (fun 
        (type_var, (variant, inj)) -> 
          String.capitalize_ascii (Option.get (Pcaml.unvala type_var))) 
      td_params 
  in
  let td_params_string = 
    if List.length td_params == 0 then "" else "[" ^ String.concat ", " td_params ^ "]"  
  in 
  match td_def with
  [ <:ctyp:< [ $list:subtypes_list$ ] >> -> 
    let abstract_class = 
      pprintf pc "sealed abstract class %s%s {}" 
        td_id 
        td_params_string
    in
    let case_class pc subtype = 
      print_constructor pc 
        "" 
        (pprintf pc "extends %s%s" 
          td_id td_params_string) 
        td_params_string 
        (List.length td_params = 0) 
        subtype
    in
      pprintf pc "%s@ %p"
        abstract_class
        (vlist case_class) subtypes_list
  | <:ctyp:< { $list:params$ } >> ->
    let get_label (_, label, _, _, _) = label in
    let labels = List.map (fun param -> get_label param) params in
    let is_mutable = 
      List.exists 
        (fun (_, _, is_mutable, _, _) -> is_mutable) params 
    in
    let params_length = List.length labels in
    let param_to_string pc (_, label, is_mutable, param_type, _) = 
      pprintf pc "%s%s: %p"
        (if is_mutable then "var " else "")
        label
        ctyp param_type
    in
      do{
        known_records.val := 
          List.cons 
            ((params_length, labels), (td_id, is_mutable)) 
            (known_records.val);
        print_case_class pc 
          "sealed "
          td_id 
          td_params_string 
          params 
          param_to_string 
          "";}
  | _ -> 
    pprintf pc "type %s%s = %p" 
      td_id
      td_params_string
      ctyp td_def
  ]
; 

(* Let statement *)

value let_in_index = ref 0;

value rec is_mutable pc expression = 
  match expression with 
  [ <:expr< [| $list:el$ |] >> -> True
  | <:expr< {$list:lpe$} >> ->
    let labels = List.map (fun (p, e) -> simple_patt pc p) lpe in
    let matching_record_option = get_known_record_option pc labels in
    match matching_record_option with 
      [ Some((_, (_, is_mutable))) -> is_mutable
      | None -> False ]
  | <:expr< if $e1$ then $e2$ else $e3$ >> -> 
    is_mutable pc e2 ||is_mutable pc e3
  | <:expr:< match $e1$ with [ $list:lpee$ ] >> ->
    List.exists (fun (_, _, e) -> is_mutable pc e) lpee
  | <:expr< ($list:el$) >> ->
    List.exists (fun e -> is_mutable pc e) el
  | _ -> False ]
;

value collect_generic_types patt_list = 
  let collect_type t = 
    loop [] t where rec loop tl t =
      match t with 
        [ <:ctyp:< ' $s$ >> -> tl @ [String.capitalize_ascii s]
        | <:ctyp:< $t1$ -> $t2$ >> -> 
          (loop tl t1) @ (loop [] t2)
        | <:ctyp< ($list:tuple_types$) >> ->
          let tuple_types_list = 
            List.flatten ( 
                List.map (fun t -> loop [] t) tuple_types )
          in
          tuple_types_list
        | <:ctyp:< $t1$ $t2$ >> ->
          (loop tl t1) @ (loop [] t2)
        | _ -> [] ]
  in
  let collect_types p = 
    match p with
      [ <:patt< ($p$ : $t$) >> -> 
        collect_type t
      |  _ -> [] ]
  in
    List.flatten (
      List.map (fun p -> collect_types p) patt_list )
;

value generic_types_list patt_list = 
  let keep_uniques x xs = 
    if List.mem x xs then xs else [x :: xs]
  in
  let remove_duplicates xs = 
    List.fold_right keep_uniques xs []
  in
    remove_duplicates (collect_generic_types patt_list)
;

value let_binding pc (p, e, attrs) is_rec =
  let (args, e) =
    match p with
    [ <:patt< ($_$ : $_$) >> -> ([], e) 
    | _ -> expr_fun_args e ] 
  in
  let is_function = List.length args > 0 in
  let def_or_var_or_val = 
    if is_rec || is_function 
      then "def" 
    else if (is_mutable pc e) then "var" 
    else "val"
  in
  let (p, e, tyo) =
    match (p, e) with 
    [ (<:patt< (_ : $_$) >>, _)  -> (p, e, None)
    | (<:patt< _ >>, _)  -> (p, e, None)
    | (<:patt< ($_$ : $_$) >>, _) -> (p, e, None)
    | (_, <:expr< ( $e$ : $t$ ) >>) -> (p, e, Some t)
    | _ -> (p, e, None) ]
  in
  let patt_tycon_when_is_function tyo pc p =
    match tyo with
    [ Some t -> 
        pprintf pc "%p): %p" 
          simple_patt p 
          ctyp t
    | None -> 
        pprintf pc "%p)" 
          simple_patt p ]
  in
  let patt_tycon tyo pc p =
    match tyo with
    [ Some t -> 
        pprintf pc "%p: %p" 
          simple_patt p 
          ctyp t
    | None -> simple_patt pc p ]
  in
  let generic_types = 
    List.map (fun t -> (t, ",")) 
      (generic_types_list args) 
  in
  let has_generic_types = List.length generic_types > 0
  in
  let print_generic_type pc p = pprintf pc "%s" p
  in 
  let args = List.map (fun p -> (p, ",")) args in 
  let pl = [(p, "") :: args] in
  let pc = {(pc) with dang = ""} in
  match pc.aft with
  [ _ when is_function ->
    pprintf pc "%s %p%s%p%s(%p = {@;%p@ }" 
      def_or_var_or_val
      simple_patt p
      (if has_generic_types then "[" else "")
      (plist print_generic_type 4) generic_types
      (if has_generic_types then "]" else "")
      (plistl simple_patt (patt_tycon_when_is_function tyo) 4) args
      expr e
  | _ ->
    pprintf pc "%s %p = %p" 
      def_or_var_or_val
      (plistl simple_patt (patt_tycon tyo) 4) pl
      expr e
  ]
;

value rec_binding pc (p, e, attrs) = let_binding pc (p, e, attrs) True;
value non_rec_binding pc (p, e, attrs) = let_binding pc (p, e, attrs) False; 

(* List *)

value rec make_expr_list =
  fun
  [ <:expr< [$x$ :: $y$] >> ->
      let (xl, c) = make_expr_list y in
      ([x :: xl], c)
  | <:expr< [] >> -> ([], None)
  | x -> ([], Some x) ]
;

value rec make_patt_list =
  fun
  [ <:patt< [$x$ :: $y$] >> ->
      let (xl, c) = make_patt_list y in
      ([x :: xl], c)
  | <:patt< [] >> -> ([], None)
  | x -> ([], Some x) ]
;

EXTEND_PRINTER
  pr_expr:
    [ "top"
      [ <:expr:< do { $list:el$ } >> as ge ->
        let statements =
          match flatten_sequence ge with
          [ Some el -> el
          | None -> el ]
        in
          pprintf pc "@[<a>{@;%p@ }@]"
            (vlist expr) statements ]
    | "expr1"
      [  <:expr< if $e1$ then $e2$ else $e3$ >> as ge ->
        let rec get_else_if e3 = 
          fun
          [ <:expr< if $e1$ then $e2$ else $e3$ >> ->
              let (eel, e3) = get_else_if e3 in
              ([(e1, e2) :: eel], e3)
          | e -> ([], e) ] e3
        in
        let (else_ifs_expr, else_expr) = get_else_if e3 in 
        horiz_vertic
          (fun () -> 
            if List.length else_ifs_expr > 0 then sprintf "\n" 
            else 
              match e3 with
              [ <:expr< $uid:"()"$ >> -> (* No else condition *)
                if pc.dang = "else" then next pc ge
                else pprintf pc "if (%p) %p" curr e1 curr e2
              | _ ->
                pprintf pc "if (%p) %p else %p" curr e1 curr e2 curr e3 ])
          (fun () ->
            let else_ifs_to_string curr pc (else_ifs, else_expr) = 
              loop pc else_ifs where rec loop pc =
                fun
                [ [(e1, e2) :: remaining_else_ifs] ->
                    pprintf pc "@[<b>else if (@;%p@ ) {@;%p @ } %p@]"
                      curr e1
                      expr1 e2
                      (loop) remaining_else_ifs
                | [] ->
                    pprintf pc "@[<b>else {@;%p@ }@]" curr else_expr ]
            in
            match else_expr with
            [ <:expr< () >> -> 
              if pc.dang = "else" then next pc ge
              else
                pprintf pc "@[<b>@;if (@;<1 4>%p@;) {@;<1 4>%p @;}@]"
                  curr e1 
                  expr1 e2
             | _ ->
                pprintf pc "@[<b>@;if (@;<1 4>%p@;) {@;<1 4>%p @;<1 2>} %p@]"
                  curr e1 
                  expr1 e2
                  (else_ifs_to_string curr) (else_ifs_expr, else_expr) ])
      | <:expr:< fun [ $list:lpee$ ] >> -> 
        match lpee with
          [ [(p1, <:vala< None >>, e1)] when is_irrefut_patt p1 ->
            let (pl, e1) = expr_fun_args e1 in
            if List.length pl > 0 then
              let pl = [p1 :: pl] in
              let pl = List.map (fun p -> (p, ",")) pl in 
                pprintf pc "@[<b>{@;@[<a>case (%p) =>@;@[{@;%p@]@ }@]@ }@]"
                  (plist patt 4) pl  
                  expr e1
            else 
              match p1 with 
              [ <:patt< ($list:pl$) >> -> 
                pprintf pc "@[<b>{@;@[<a>case %p =>@;@[{@;%p@]@ }@]@ }@]"
                  patt p1 
                  expr e1
              | _ ->  
                pprintf pc "@[<a>(@;@[(%p) =>@;@[{@;%p@]@ }@]@ )@]"
                  simple_patt p1 
                  expr e1 ] 
          | lpee ->
            pprintf pc "@[<b>(@;@[x =>@;@[x match {@;%p@ @]}@]@ )@]" 
              (vlist match_case) lpee ]
      | <:expr:< match $e1$ with [ $list:lpee$ ] >> ->
        pattern_matching pc e1 lpee
      | <:expr:< let $flag:rf$ $list:pel$ in $e$ >> ->
        let binding = if rf then rec_binding else non_rec_binding in
        let index = let_in_index.val in
          do {
            let_in_index.val := index + 1;
            pprintf pc "@[<b>val _%d = {@;%p@;%p@ }@]"
              index
              (vlist binding) pel
              expr e } 
      | <:expr:< while $e1$ do { $list:el$ } >> ->
        pprintf pc "@[<a>@[<a>while (@;%p@ ) {@]@;%p@ }@]" 
          curr e1
          (vlist expr) el
      | <:expr:< for $v$ = $e1$ $to:is_increasing$ $e2$ do { $list:el$ } >> ->
        pprintf pc "@[<a>@[<a>for (@;%p <-@;@[<a>%p to %p%s@]@ ) {@]@;%p@ }@]"
          patt v
          curr e1
          curr e2
          (if is_increasing then "" else " by -1")
          (vlist expr) el ]
    | "tuple"
      [ <:expr< ($list:el$) >> as z -> (* Max size of tuples in scala is 22 *)
        if List.length el <= 22 then 
          let el = List.map (fun e -> (e, ",")) el 
          in
            pprintf pc "(%p)" (plist curr 0) el 
        else 
          let fail() = 
          Ploc.raise (MLast.loc_of_expr z)
            (Failure (sprintf "Too many elements for tuple. \\
              Maximum 22 elements are allowed.")) 
          in
            pprintf pc "@[<1>(%p)@]" (bottom ~{fail=fail}) z ]
    | "assign"
      [ <:expr:< $x$ := $y$ >> -> 
        match x with 
        [ <:expr< $array$ .( $index$ ) >> ->  
          pprintf pc "%p.update(%p, %p)"
            next array 
            expr index 
            expr y 
        | _ ->
          pprintf pc "%p = %p"
            next x
            expr y ] ]
    | "or" 
      [ z ->
        let unfold =
          fun
          [ <:expr< $lid:op$ $x$ $y$ >> ->
              if List.mem op ["||"; "or"] then
                  Some (x, " ||", y)
              else None
          | _ -> None ]
        in
        let loc = MLast.loc_of_expr z in
        right_operator pc loc 0 unfold next z ]
    | "and"
      [ z ->
            let unfold =
              fun
              [ <:expr< $lid:op$ $x$ $y$ >> ->
                  if List.mem op ["&&"; "&"] then
                    Some (x, " &&", y)
                  else None
              | _ -> None ]
            in
            let loc = MLast.loc_of_expr z in
            right_operator pc loc 0 unfold next z ]
    | "less"
      [ <:expr:< $lid:op$ $x$ $y$ >> as z ->
        if List.mem op ["!="; "<"; "<="; "<>"; "="; "=="; ">"; ">="] || 
          is_infixop0 op then
          let newOp = 
            fun 
            [ "<>" -> "!="
            | "!=" -> "ne"
            | "=" -> "=="
            | "==" -> "eq"
            | s -> s ] op
          in
          operator pc next next 0 (loc, newOp) x y
        else next pc z ]
    | "concat"
      [ z ->
        let unfold =
          fun
          [ <:expr< $lid:op$ $x$ $y$ >> ->
              if List.mem op ["^"; "@"] || is_infixop1 op then 
                fun 
                [ "^" -> Some (x, " +", y) 
                | "@" -> Some (x, " ++", y)
                | s -> None ] op
              else None
          | _ -> None ]
        in
        let loc = MLast.loc_of_expr z in
        right_operator pc loc 0 unfold next z ]
    | "cons"
      [ <:expr< [$_$ :: $_$] >> as z ->
        let (xl, y) = make_expr_list z in
        match y with
        [ Some y ->
            let xl = List.map (fun x -> (x, " ::")) (xl @ [y]) in
            plist next 0 pc xl
        | None -> next pc z ] ]
    | "add" 
      [ z ->
        let ops = ["+"; "+."; "-"; "-."] in
        let unfold =
          fun
          [ <:expr< $lid:op$ $x$ $y$ >> ->
              if List.mem op ops || is_infixop2 op then 
                Some (x, " " ^ (String.sub op 0 1), y) 
              else None
          | _ -> None ]
        in
        let loc = MLast.loc_of_expr z in
        left_operator pc loc 0 unfold next z ] 
    | "mul" 
      [ z ->
        let ops = ["*"; "*."; "/"; "/."; "land"; "lor"; "lxor"; "mod"] in
        let unfold =
          fun
          [ <:expr< $lid:op$ $x$ $y$ >> ->
            if List.mem op ops || is_infixop3 op then

              fun 
              [ "*" | "*." -> Some (x, " *", y)
              | "/" | "/." -> Some (x, " /", y)
              | "land" -> Some (x, " &", y)
              | "lor" -> Some (x, " |", y)
              | "lxor" -> Some (x, " ^", y)
              | "mod" -> Some (x, " %", y)
              | s -> None ] op
              
            else None
          | _ -> None ]
        in
        let loc = MLast.loc_of_expr z in
        left_operator pc loc 0 unfold next z ]
    | "pow" 
      [ z ->
          let ops = ["asr"; "lsl"; "lsr"] in
          let unfold =
            fun
            [ <:expr< $lid:op$ $x$ $y$ >> ->
              if List.mem op ops || is_infixop4 op then 
                fun 
                [ "asr" -> Some (x, " >>>", y)
                | "lsl" -> Some (x, " <<", y)
                | "lsr" -> Some (x, " >>", y)
                | s -> None ] op
              else None
            | _ -> None ]
          in
          let loc = MLast.loc_of_expr z in
          right_operator pc loc 0 unfold next z ]
    | "unary_minus"
      [ <:expr< $lid:op$ $x$ >> as z ->
        let ops = [("-","-") ; ("-.","-"); ("~+","+"); ("~+.","+")] in
        let in_ops x = List.mem_assoc x ops in
        if in_ops op then
          pprintf pc "%s(%p)" (List.assoc op ops) expr x
        else next pc z]
    | "apply" 
      [ <:expr< assert $e$ >> ->
        pprintf pc "@[<a>assert(@;<0 2>%p@;<0 0>)@]" expr e
      | <:expr:< $_$ $_$ >> as z ->
        let inf =
          match z with
          [ <:expr< $lid:n$ $_$ $_$ >> -> is_infix n || is_infix_operator n
          | <:expr< [$_$ :: $_$] >> -> True
          | _ -> False ]
        in
        if inf then next pc z
        else 
          let unfold = 
            fun
            [ <:expr< [$_$ :: $_$] >> -> None
            | <:expr< $lid:n$ $_$ $_$ >> when is_infix n || is_infix_operator n -> 
              None
            | <:expr< $lid:n$ $_$ >> when is_unary n || is_prefixop n -> None
            | <:expr< $x$ $y$ >> -> Some (x, ",", y)
            | e -> None ]
          in
          let (fun_name, args) = 
            loop [] z "" where rec loop args_list first_arg op =
              match unfold first_arg with
              [ Some (x1, op1, x2) -> loop [(x2, op) :: args_list] x1 op1
              | None ->  (first_arg, args_list)]
          in
          let get_main_arg (main_arg, _) = main_arg in 
          let print_params pc (has_params, not_arg_index) = 
            (if has_params then 
              pprintf pc "(%p)" 
                (plist expr 0) (List.filteri (fun i a -> i != not_arg_index) args) 
            else pprintf pc "") 
          in
          let expr_or_tuple pc e = 
            match e with 
            [ <:expr< ($list:el$) >> -> 
              let el = List.map (fun e -> (e, ",")) el
              in
                pprintf pc "%p" (plist expr 0) el 
            | _ -> expr pc e ]
          in
          let print_known_applied_function (fun_name, index, has_params) = 
            pprintf pc "%p%s%p"
              expr (get_main_arg (List.nth args index))
              fun_name
              print_params (has_params, index)
          in
          let print_known_function fun_name = 
            horiz_vertic 
              (fun () -> 
                pprintf pc "%s(%p)"
                  fun_name 
                  (hlist (op_after expr_or_tuple)) args)
              (fun () -> 
                pprintf pc "%s(@;%p)"
                  fun_name 
                  (plist expr_or_tuple 0) args)
          in
          let default_print = 
            horiz_vertic 
              (fun () -> 
                pprintf pc "%p(%p)"
                  expr fun_name 
                  (hlist (op_after expr_or_tuple)) args)
              (fun () -> 
                pprintf pc "%p(@;%p)"
                  expr fun_name 
                  (plist expr_or_tuple 0) args)
          in
            match fun_name with
            [ <:expr< List . $lid:v$ >> -> 
              if v = "flatten" || v = "concat" then
                print_known_function "ListOps.flatten"
              else
                (* Map list functions to 
                  (name in scala, which arg it is applied to, it has parameters) *)
                let list_funs = 
                  [ ("length", (".length", 0, False)); 
                    ("cons", (" ::", 0, True));
                    ("hd", (".head", 0, False));
                    ("tl", (".tail", 0, False));
                    ("nth", (".apply", 0, True));
                    ("rev", (".reverse", 0, False));
                    ("append", (" ++", 0, True));
                    ("rev_append", (" reverse_:::", 0, True));
                    ("map", (".map", 1, True));
                    ("for_all", (".forall", 1, True));
                    ("exists", (".exists", 1, True));
                    ("mem", (".contains", 1, True));
                    ("find_opt", (".find", 1, True));
                    ("filter", (".filter", 1, True)) ] 
                in
                  match List.assoc_opt v list_funs with 
                  [ Some scala_fun -> 
                    print_known_applied_function scala_fun
                  | None -> 
                    default_print ]
            | <:expr< Option . $lid:v$ >> ->
              if v = "some" then
                print_known_function "Some"
              else
                let options_funs = 
                  [ ("get", (".get", 0, False));
                    ("is_none", (".isEmpty", 0, False));
                    ("is_some", (".isDefined", 0, False));
                    ("map", (".map", 1, True)) ] 
                in
                  match List.assoc_opt v options_funs with 
                  [ Some scala_fun -> 
                    print_known_applied_function scala_fun
                  | None -> 
                    default_print ]
            | <:expr< $lid:v$ >> -> 
              if v = "raise" then 
                let (exception_name, args) = 
                  match List.hd args with 
                    [ (<:expr< $_$ $_$ >>, _) as with_params -> 
                      let (with_params, sep) = with_params in
                      let (exception_name, args) = 
                        loop [] with_params where rec loop args_list first_arg =
                          match first_arg with
                          [ <:expr< $x$ $y$ >> -> loop [y :: args_list] x
                          | _ -> (first_arg, args_list)]
                      in
                        (exception_name , args)
                    | (<:expr< $_$ >>, _) as simple ->
                      let (simple, sep) = simple in
                      (simple, [])]
                in
                  pprintf pc "@[<a>assert(@;<0 2>false,@;\"%p%s\")@]"
                    expr exception_name
                    (if List.length args > 0 then 
                      " with " ^ (String.concat ", " (List.map (fun a -> 
                        match a with 
                          [ <:expr< $str:a$ >> -> pprintf pc "%s" a
                          | _ -> expr pc a]) args))
                    else "")
              else 
                let known_funs = 
                  [("print_string", ("stainless.io.StdOut.print")); 
                   ("print_endline", ("println"))] 
                in
                  match List.assoc_opt v known_funs with 
                  [ Some scala_fun -> 
                    print_known_function scala_fun
                  | None -> 
                    default_print ]
            | _ -> default_print ] ]
    | "dot" 
      [ <:expr:< $e$ . $lid:s$ >> -> 
        pprintf pc "%p.@;<0 0>%s"
          curr e
          s
      | <:expr< $x$ .( $y$ ) >> -> (* For Array access *)
        pprintf pc "@[<a>%p(@;<0 2>%p@;<0 0>)@]"
          expr x
          expr y
      | <:expr< $x$ .[ $y$ ] >> -> (* For strings access *)
        pprintf pc "@[<a>%p(@;<0 2>%p@;<0 0>)@]"
          expr x
          expr y ]
    | "simple" 
      [ <:expr< {$list:lpe$} >> ->
        let labels_to_values = 
          List.map (fun (p, e) -> (simple_patt pc p, e)) lpe 
        in
        let labels = 
          List.map (fun (l, v) -> l) labels_to_values
        in 
        let matching_record_option = 
          get_known_record_option pc labels
        in
        match matching_record_option with
          [ Some ((_, record_labels), (record_name, _)) ->
            let to_value pc label = 
              pprintf pc "%p" 
                expr (List.assoc label labels_to_values) 
            in
              horiz_vertic 
                (fun () -> 
                  pprintf pc "@[<a>%s(@;<0 2>%p@;<0 0>)@]" 
                  record_name
                  (hlistl (comma_after to_value) to_value) record_labels)
                (fun () -> 
                  pprintf pc "@[<a>%s(@;<0 2>%p@;<0 0>)@]" 
                  record_name
                  (vlistl (comma_after to_value) to_value) record_labels)
          | None -> 
            pprintf pc " OUPS NO RECORD FOUND "]
      | <:expr< {($e$) with $list:lpe$} >> ->
        let lpe = List.map (fun pe -> (pe, ",")) lpe in
        let print_new_binding pc (label, new_value) =
          pprintf pc "%p =@;%p"
            simple_patt label
            expr new_value 
        in
          pprintf pc "@[<a>%p.copy(@;<0 2>%p@;<0 0>)@]" 
            expr e
            (plist print_new_binding 0) lpe
      | <:expr< [| $list:el$ |] >> ->
        if el = [] then pprintf pc "Array()"
        else
          let el = List.map (fun e -> (e, ",")) el in
          pprintf pc "@[<1>Array(%p)@]" 
            (plist expr 0) el
      | <:expr< [$_$ :: $_$] >> as z ->
        let (xl, y) = make_expr_list z in
        match y with
        [ Some _ -> pprintf pc "@[<1>(%p)@]" expr z
        | None ->
            let xl = List.map (fun x -> (x, ",")) xl in
            pprintf pc "@[<1>List(%p)@]" (plist expr1 0) xl ]
      | <:expr< $int:s$ >> ->
        print_int63 pc s
      | <:expr< $flo:s$ >> ->
        if s.[String.length s - 1] = '.' then  
          pprintf pc "%s0" s
        else pprintf pc "%s" s
      | <:expr:< $lid:s$ >> -> 
        lids pc s
      | <:expr< $chr:s$ >> -> 
        pprintf pc "'%s'" (ocaml_char s)
      | <:expr< $str:s$ >> -> 
        pprintf pc "\"%s\"" s
      | <:expr< $uid:s$ >> -> 
        uids pc s ]
    | "bottom"
      [ z ->
        let fail() = 
        Ploc.raise (MLast.loc_of_expr z)
          (Failure 
            (sprintf 
              "The translation to Scala has failed due to an unrecognized\\
               expression.")) 
        in
          pprintf pc "@[<1>(%p)@]" (bottom ~{fail=fail}) z ] 
    ];
  pr_patt: 
    [ "or"
      [ <:patt:< $_$ | $_$ >> as z ->
          let unfold =
            fun
            [ <:patt< $x$ | $y$ >> -> Some (x, " |", y)
            | _ -> None ]
          in
          left_operator pc loc 0 unfold next z ]
    | "tuple"
      [ <:patt< ($list:pl$) >>  as z ->
        if List.length pl <= 22 then 
          let pl = List.map (fun p -> (p, ",")) pl 
          in
            pprintf pc "(%p)" (plist curr 0) pl 
        else 
          let fail() = 
          Ploc.raise (MLast.loc_of_patt z)
            (Failure 
              (sprintf 
                "Too many elements for tuple. Maximum 22 elements are allowed.")) 
          in
            pprintf pc "@[<1>(%p)@]" (bottom ~{fail=fail}) z ]
    | "cons"
      [ <:patt< [$_$ :: $_$] >> as z ->
        match z with   
        [ <:patt< [$x$ :: $y$] >> ->
          pprintf pc "Cons(%p, %p)"
              next x
              curr y
        | <:patt< [] >> -> next pc z 
        | _ -> next pc z ] ]
    | "apply"
      [ <:patt< $_$ $_$ >> as z ->
        let case_class_with_params_opt =
          loop [] z where rec loop params =
            fun
            [ <:patt< $x$ $y$ >> -> loop [y :: params] x
            | <:patt< $uid:"::"$ >> -> None
            | case_class -> Some (case_class, params) ]
        in
        match case_class_with_params_opt with
        [ None -> next pc z
        | Some (case_class, params) -> 
            let params = List.map (fun param -> (param, ",")) params 
            in
              pprintf pc "%p@[(%p)@]" 
                next case_class 
                (plist patt 0) params ] ]
    | "simple" 
      [ ]
    | "atomic" 
      [ <:patt< [$_$ :: $_$] >> as z ->
        let (xl, y) = make_patt_list z in
        match y with
        [ Some y -> pprintf pc "@[<1>(%p)@]" patt z
        | None ->
            let xl = List.map (fun x -> (x, ",")) xl in
            pprintf pc "@[<1>List(%p)@]" (plist patt 0) xl ]
      | <:patt< ($p$ : $t$) >> ->
        pprintf pc "%p: %p" patt p ctyp t
      | <:patt< $int:s$ >> -> 
        print_int63 pc s
      | <:patt< $flo:s$ >> ->
        if s.[String.length s - 1] = '.' then  
          pprintf pc "%s0" s
        else pprintf pc "%s" s
      | <:patt:< $lid:s$ >> ->
        lids pc s
      | <:patt:< $uid:s$ >> ->
        uids pc s
      | <:patt< $chr:s$ >> -> 
        pprintf pc "'%s'" (ocaml_char s)
      | <:patt< $str:s$ >> -> 
        pprintf pc "\"%s\"" s
      | <:patt< _ >> -> 
        pprintf pc "_"]
    | "bottom"
      [ z ->
        let fail() = 
        Ploc.raise (MLast.loc_of_patt z)
          (Failure 
            (sprintf 
              "The translation to Scala has failed due to an unrecognized\\
               pattern.")) 
        in
          pprintf pc "@[<1>(%p)@]" (bottom ~{fail=fail}) z ] 
    ];
  pr_ctyp: 
    [ "arrow"
      [ <:ctyp:< $t1$ -> $t2$ >> as z ->
        let tl =
          loop [] z where rec loop tl t =
            match t with
            [ <:ctyp< $t1$ -> $t2$ >> -> loop [t1 :: tl] t2
            | _ -> [t :: tl] ]
        in
        let returnType = List.hd tl in
        let argsType = List.rev (List.tl tl) in
        let argsType = List.map (fun t -> (t, ",")) argsType in
        let has_parentheses = List.length argsType > 1 in
          pprintf pc "%s%p%s => %p"
            (if has_parentheses then "(" else "") 
            (plist ctyp 0) argsType
            (if has_parentheses then ")" else "")
            next returnType ]
    | "star"
      [ <:ctyp< ($list:tl$) >> ->
        let tl = List.map (fun t -> (t, ",")) tl 
        in
          pprintf pc "(%p)"
            (plist curr 2) tl ]
    | "apply"
      [ <:ctyp:< $t1$ $t2$ >> ->
        match t1 with
        [ <:ctyp< $_$ $_$ >> ->
          let (t, tl) =
            loop [t2] t1 where rec loop args =
              fun
              [ <:ctyp< $x$ $y$ >> -> loop [y :: args] x
              | t -> (t, args) ]
          in
          let tl = List.map (fun t -> (t, ",")) tl 
          in
            pprintf pc "%p[%p]" 
              ctyp t
              (plist ctyp 2) tl
        | _ ->
            pprintf pc "%p[%p]" ctyp t1 ctyp t2 ] ]
    | "simple" 
      [ <:ctyp:< $lid:t$ >> ->
        fun [
            "int" -> pprintf pc "Int63"
          | "float" -> pprintf pc "Double"
          | "bool" -> pprintf pc "Boolean"
          | "char" -> pprintf pc "Char"
          | "string" -> pprintf pc "String"
          | "unit" -> pprintf pc "Unit"
          | _ -> pprintf pc "%s" (String.capitalize_ascii t)
        ] t
      | <:ctyp:< ' $s$ >> ->
        pprintf pc "%s" (String.capitalize_ascii s) ]
    | "bottom"
      [ z ->
        let fail() = 
        Ploc.raise (MLast.loc_of_ctyp z)
          (Failure 
            (sprintf 
              "The translation to Scala has failed due to an unrecognized type.")) 
        in
          pprintf pc "@[<1>(%p)@]" (bottom ~{fail=fail}) z ] 
    ];
  pr_str_item:
    [ "top"
      [ <:str_item:< exception $excon:ec$ $_itemattrs:item_attrs$ >> as z ->
        match ec with 
          [ MLast.EcTuple _ gc -> 
            print_constructor pc "sealed " "extends Exception" "" False gc
          | _ -> 
            let fail() = 
            Ploc.raise (MLast.loc_of_str_item z)
              (Failure 
                (sprintf 
                  "The translation to Scala has failed due to an unrecognized\\
                   exception declaration.")) 
            in
              pprintf pc "@[<1>(%p)@]" (bottom ~{fail=fail}) z ] 
      | <:str_item:< type $flag:nonrf$ $list:tdl$ >> ->
        pprintf pc "%p"
          (vlist2 type_decl (double_line_return_before type_decl)) tdl
      | <:str_item:< value $flag:is_rec$ $list:and_separated_pel$ >> -> 
        let binding = if is_rec then rec_binding else non_rec_binding in
          pprintf pc "%p"
            (vlist binding) and_separated_pel
      | <:str_item< $exp:e$ >> ->
        pprintf pc "%p" expr e ]
    | "bottom"
      [ z ->
        let fail() = 
        Ploc.raise (MLast.loc_of_str_item z)
          (Failure 
            (sprintf 
              "The translation to Scala has failed due to an unrecognized\\
               structure item.")) 
        in
          pprintf pc "@[<1>(%p)@]" (bottom ~{fail=fail}) z ] 
    ];
  END;

(* Main program *)

value sep = Pcaml.inter_phrases;

value output_string_eval oc s =
  loop 0 where rec loop i =
    let length = String.length s in
    if i == length then ()
    else if i == length - 1 then output_char oc s.[i]
    else if i == length - 2 then 
      do { 
        output_char oc s.[i]; 
        output_char oc s.[i + 1]
      }
    else
      match (s.[i], s.[i + 1], s.[i + 2]) with
      [ ('(', '*', '*') -> do { output_string oc "/*"; loop (i + 3) }
      | ('(', '*', _) -> do { output_string oc "/*"; loop (i + 2) }
      | (c, '*', ')') -> 
        do { 
          output_char oc c; 
          output_string oc "*/"; 
          loop (i + 3) 
        }
      | ('\\', 'n', _) -> do { output_char oc '\n'; loop (i + 2) }
      | (c, '\\', 'n') -> 
        do { 
          output_char oc c; 
          output_char oc '\n';
          loop (i + 3) 
        }
      | (c, _, _) -> do { output_char oc c; loop (i + 1) } ]
;

value apply_printer f (ast, eoi_loc) = do {
  let oc = do { pervasives_set_binary_mode_out stdout True; stdout } 
  in
  try do {
    let _ =
      List.fold_left
        (fun first (si, loc) -> do {
           match sep.val with
           [ Some str -> 
              if first then () else output_string_eval oc str
           | None -> output_string_eval oc (Ploc.comment loc) ];
           flush oc;
           output_string oc (f {ind = 0; bef = ""; aft = ""; dang = ""} si);
           False
         })
        True ast
    in
    output_string_eval oc (Ploc.comment eoi_loc);
    flush oc
  }
  with exn -> do {
    raise exn
  };
};

Pcaml.print_implem.val := apply_printer str_item;
