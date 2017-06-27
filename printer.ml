(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Projet Cristal, INRIA Rocquencourt                   *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format
open Outcometree

exception Ellipsis
let ellipsis ppf = fprintf ppf "…"

let cautious f ppf arg =
  try f ppf arg with
    Ellipsis -> ellipsis ppf

let rec ident ppf =
  function
    Oide_ident s -> pp_print_string ppf s
  | Oide_dot (id, s) ->
      ident ppf id; pp_print_char ppf '.'; pp_print_string ppf s
  | Oide_apply (id1, id2) ->
      fprintf ppf "%a(%a)" ident id1 ident id2

let parenthesized_ident name =
  (List.mem name ["or"; "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"])
  ||
  (match name.[0] with
      'a'..'z' | 'A'..'Z' | '\223'..'\246' | '\248'..'\255' | '_' ->
        false
    | _ -> true)

let value_ident ppf name =
  if parenthesized_ident name then
    fprintf ppf "( %s )" name
  else
    pp_print_string ppf name

(* Values *)

let valid_float_lexeme s =
  let l = String.length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match s.[i] with
    | '0' .. '9' | '-' -> loop (i+1)
    | _ -> s
  in loop 0

let float_repres f =
  match classify_float f with
    FP_nan -> "nan"
  | FP_infinite ->
      if f < 0.0 then "-∞" else "∞"
  | _ ->
      let float_val =
        let s1 = Printf.sprintf "%.12g" f in
        if f = float_of_string s1 then s1 else
        let s2 = Printf.sprintf "%.15g" f in
        if f = float_of_string s2 then s2 else
        Printf.sprintf "%.18g" f
      in valid_float_lexeme float_val

let parenthesize_if_neg ppf fmt v isneg =
  if isneg then pp_print_char ppf '(';
  fprintf ppf fmt v;
  if isneg then pp_print_char ppf ')'

let value ppf tree =
  let rec print_tree_1 ppf =
    function
    | Oval_constr (name, [param]) ->
        fprintf ppf "@[<1>%a@ %a@]" ident name print_constr_param param
    | Oval_constr (name, (_ :: _ as params)) ->
        fprintf ppf "@[<1>%a@ (%a)@]" ident name
          (print_tree_list print_tree_1 ",") params
    | Oval_variant (name, Some param) ->
        fprintf ppf "@[<2>`%s@ %a@]" name print_constr_param param
    | tree -> print_simple_tree ppf tree
  and print_constr_param ppf = function
    | Oval_int i -> parenthesize_if_neg ppf "%i" i (i < 0)
    | Oval_int32 i -> parenthesize_if_neg ppf "%lil" i (i < 0l)
    | Oval_int64 i -> parenthesize_if_neg ppf "%LiL" i (i < 0L)
    | Oval_nativeint i -> parenthesize_if_neg ppf "%nin" i (i < 0n)
    | Oval_float f -> parenthesize_if_neg ppf "%s" (float_repres f) (f < 0.0)
    | tree -> print_simple_tree ppf tree
  and print_simple_tree ppf =
    function
      Oval_int i -> fprintf ppf "%i" i
    | Oval_int32 i -> fprintf ppf "%lil" i
    | Oval_int64 i -> fprintf ppf "%LiL" i
    | Oval_nativeint i -> fprintf ppf "%nin" i
    | Oval_float f -> pp_print_string ppf (float_repres f)
    | Oval_char c -> fprintf ppf "%C" c
    | Oval_string s ->
        begin try fprintf ppf "%S" s with
          Invalid_argument _ (* "String.create" *)-> fprintf ppf "<huge string>"
        end
    | Oval_list tl ->
        fprintf ppf "@[<1>[%a]@]" (print_tree_list print_tree_1 ";") tl
    | Oval_array tl ->
        fprintf ppf "@[<2>⟦%a⟧@]" (print_tree_list print_tree_1 ";") tl
    | Oval_constr (name, []) -> ident ppf name
    | Oval_variant (name, None) -> fprintf ppf "`%s" name
    | Oval_stuff s -> pp_print_string ppf s
    | Oval_record fel ->
        fprintf ppf "@[<1>{%a}@]" (cautious (print_fields true)) fel
    | Oval_ellipsis -> raise Ellipsis
    | Oval_printer f -> f ppf
    | Oval_tuple tree_list ->
        fprintf ppf "@[<1>(%a)@]" (print_tree_list print_tree_1 ",") tree_list
    | tree -> fprintf ppf "@[<1>(%a)@]" (cautious print_tree_1) tree
  and print_fields first ppf =
    function
      [] -> ()
    | (name, tree) :: fields ->
        if not first then fprintf ppf ";@ ";
        fprintf ppf "@[<1>%a@ =@ %a@]" ident name (cautious print_tree_1)
          tree;
        print_fields false ppf fields
  and print_tree_list print_item sep ppf tree_list =
    let rec print_list first ppf =
      function
        [] -> ()
      | tree :: tree_list ->
          if not first then fprintf ppf "%s@ " sep;
          print_item ppf tree;
          print_list false ppf tree_list
    in
    cautious (print_list true) ppf tree_list
  in
  cautious print_tree_1 ppf tree

(* Types *)

let rec list_init pr sep ppf =
  function
    [] -> ()
  | a :: l -> sep ppf; pr ppf a; list_init pr sep ppf l

let rec list pr sep ppf =
  function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; sep ppf; list pr sep ppf l

let pr_present =
  list (fun ppf s -> fprintf ppf "`%s" s) (fun ppf -> fprintf ppf "@ ")

let tyvar ng ppf t =
  let ng = if ng then "_" else "" in
  let n = String.length t in
  if t = "_" then fprintf ppf "_"
  else if n < 3 then
    begin
      fprintf ppf "%s" ng;
      for i = 0 to (n-1) do
        match t.[i] with
        | 'A'..'Z' | 'a'..'z' as a ->
          fprintf ppf
          "\206%c" (Char.chr @@ Char.code a - Char.code 'A' + 145)
        | x -> fprintf ppf "%c" x
      done
    end
  else
    fprintf ppf "'%s%s" ng t

let pr_vars =
  list (tyvar false) (fun ppf -> fprintf ppf "@ ")

let rec type' ppf =
  function
  | Otyp_alias (ty, s) ->
      fprintf ppf "@[%a@ as %a@]" type' ty (tyvar false) s
  | Otyp_poly (sl, ty) ->
      fprintf ppf "@[<hov 2>%a.@ %a@]"
        pr_vars sl
        type' ty
  | ty ->
      type_1 ppf ty

and type_1 ppf =
  function
    Otyp_arrow (lab, ty1, ty2) ->
      pp_open_box ppf 0;
      if lab <> "" then (pp_print_string ppf lab; pp_print_char ppf ':');
      type_2 ppf ty1;
      pp_print_string ppf " ⟶";
      pp_print_space ppf ();
      type_1 ppf ty2;
      pp_close_box ppf ()
  | ty -> type_2 ppf ty
and type_2 ppf =
  function
    Otyp_tuple tyl ->
      fprintf ppf "@[<0>%a@]" (typlist simple_type " *") tyl
  | ty -> simple_type ppf ty
and simple_type ppf =
  function
    Otyp_class (ng, id, tyl) ->
      fprintf ppf "@[%a%s#%a@]" typargs tyl (if ng then "_" else "")
        ident id
  | Otyp_constr (id, tyl) ->
      pp_open_box ppf 0;
      typargs ppf tyl;
      ident ppf id;
      pp_close_box ppf ()
  | Otyp_object (xfields, rest) ->
      fprintf ppf "@[<2>⟨%a⟩@]" (fields rest) xfields
  | Otyp_stuff s -> pp_print_string ppf s
  | Otyp_var (ng, s) -> tyvar ng ppf s
  | Otyp_variant (non_gen, row_fields, closed, tags) ->
      let present ppf =
        function
          None | Some [] -> ()
        | Some l -> fprintf ppf "@;<1 -2>> @[<hov>%a@]" pr_present l
      in
      let fields ppf =
        function
          Ovar_fields fields ->
            list row_field (fun ppf -> fprintf ppf "@;<1 -2>| ")
              ppf fields
        | Ovar_name (id, tyl) ->
            fprintf ppf "@[%a%a@]" typargs tyl ident id
      in
      fprintf ppf "%s[%s@[<hov>@[<hov>%a@]%a ]@]" (if non_gen then "_" else "")
        (if closed then if tags = None then " " else "< "
         else if tags = None then " " else "? ")
        fields row_fields
        present tags
  | Otyp_alias _ | Otyp_poly _ | Otyp_arrow _ | Otyp_tuple _ as ty ->
      pp_open_box ppf 1;
      pp_print_char ppf '(';
      type' ppf ty;
      pp_print_char ppf ')';
      pp_close_box ppf ()
  | Otyp_abstract | Otyp_open
  | Otyp_sum _ | Otyp_manifest (_, _) -> ()
  | Otyp_record lbls -> record_decl ppf lbls
  | Otyp_module (p, n, tyl) ->
      fprintf ppf "@[<1>(module %s" p;
      let first = ref true in
      List.iter2
        (fun s t ->
          let sep = if !first then (first := false; "with") else "and" in
          fprintf ppf " %s type %s = %a" sep s type' t
        )
        n tyl;
      fprintf ppf ")@]"
  | Otyp_attribute (t, attr) ->
      fprintf ppf "@[<1>(%a [@@%s])@]" type' t attr.oattr_name
and record_decl ppf lbls =
  fprintf ppf "{%a@;<1 -2>}"
    (list_init label (fun ppf -> fprintf ppf "@ ")) lbls
and fields rest ppf =
  function
    [] ->
      begin match rest with
        Some non_gen -> fprintf ppf "%s‥" (if non_gen then "_" else "")
      | None -> ()
      end
  | [s, t] ->
      fprintf ppf "%s : %a" s type' t;
      begin match rest with
        Some _ -> fprintf ppf ";@ "
      | None -> ()
      end;
      fields rest ppf []
  | (s, t) :: l ->
      fprintf ppf "%s : %a;@ %a" s type' t (fields rest) l
and row_field ppf (l, opt_amp, tyl) =
  let pr_of ppf =
    if opt_amp then fprintf ppf " of@ &@ "
    else if tyl <> [] then fprintf ppf " of@ "
    else fprintf ppf ""
  in
  fprintf ppf "@[<hov 2>`%s%t%a@]" l pr_of (typlist type' " &")
    tyl
and typlist elem sep ppf =
  function
    [] -> ()
  | [ty] -> elem ppf ty
  | ty :: tyl ->
      elem ppf ty;
      pp_print_string ppf sep;
      pp_print_space ppf ();
      typlist elem sep ppf tyl
and typargs ppf =
  function
    [] -> ()
  | [ty1] -> simple_type ppf ty1; pp_print_space ppf ()
  | tyl ->
      pp_open_box ppf 1;
      pp_print_char ppf '(';
      typlist type' "," ppf tyl;
      pp_print_char ppf ')';
      pp_close_box ppf ();
      pp_print_space ppf ()
and label ppf (name, mut, arg) =
  fprintf ppf "@[<2>%s%s :@ %a@];" (if mut then "mutable " else "") name
    type arg

(* Class types *)

let type_parameter ppf (ty, (co, cn)) =
  fprintf ppf "%s%a"
    (if not cn then "+" else if not co then "-" else "")
    (tyvar false) ty

let class_params ppf =
  function
    [] -> ()
  | tyl ->
      fprintf ppf "@[<1>[%a]@]@ "
        (list type_parameter (fun ppf -> fprintf ppf ", "))
        tyl

let rec class_type ppf =
  function
    Octy_constr (id, tyl) ->
      let pr_tyl ppf =
        function
          [] -> ()
        | tyl ->
            fprintf ppf "@[<1>[%a]@]@ " (typlist type' ",") tyl
      in
      fprintf ppf "@[%a%a@]" pr_tyl tyl ident id
  | Octy_arrow (lab, ty, cty) ->
      fprintf ppf "@[%s%a ⟶@ %a@]" (if lab <> "" then lab ^ ":" else "")
        type_2 ty class_type cty
  | Octy_signature (self_ty, csil) ->
      let pr_param ppf =
        function
          Some ty -> fprintf ppf "@ @[(%a)@]" type' ty
        | None -> ()
      in
      fprintf ppf "@[<hov 2>@[<2>object%a@]@ %a@;<1 -2>end@]" pr_param self_ty
        (list class_sig_item (fun ppf -> fprintf ppf "@ "))
        csil
and class_sig_item ppf =
  function
    Ocsg_constraint (ty1, ty2) ->
      fprintf ppf "@[<2>constraint %a =@ %a@]" type' ty1
        type' ty2
  | Ocsg_method (name, priv, virt, ty) ->
      fprintf ppf "@[<2>method %s%s%s :@ %a@]"
        (if priv then "private " else "") (if virt then "virtual " else "")
        name type' ty
  | Ocsg_value (name, mut, vr, ty) ->
      fprintf ppf "@[<2>val %s%s%s :@ %a@]"
        (if mut then "mutable " else "")
        (if vr then "virtual " else "")
        name type' ty

(* Signature *)

let out_module_type = ref (fun _ -> failwith "Oprint.out_module_type")
let out_sig_item = ref (fun _ -> failwith "Oprint.out_sig_item")
let out_signature = ref (fun _ -> failwith "Oprint.out_signature")
let out_type_extension = ref (fun _ -> failwith "Oprint.out_type_extension")

let rec functor' funct ppf =
  function
    Omty_functor (_, None, mty_res) ->
      if funct then fprintf ppf "() %a" (functor' true) mty_res
      else fprintf ppf "functor@ () %a" (functor' true) mty_res
  | Omty_functor (name, Some mty_arg, mty_res) -> begin
      match name, funct with
      | "_", true ->
          fprintf ppf "⟶@ %a ⟶@ %a"
            module_type mty_arg (functor' false) mty_res
      | "_", false ->
          fprintf ppf "%a ⟶@ %a"
            module_type mty_arg (functor' false) mty_res
      | name, true ->
          fprintf ppf "(%s : %a) %a" name
            module_type mty_arg (functor' true) mty_res
      | name, false ->
            fprintf ppf "functor@ (%s : %a) %a" name
              module_type mty_arg (functor' true) mty_res
    end
  | m ->
      if funct then fprintf ppf "⟶@ %a" module_type m
      else module_type ppf m

and module_type ppf =
  function
    Omty_abstract -> ()
  | Omty_functor _ as t ->
      fprintf ppf "@[<2>%a@]" (functor' false) t
  | Omty_ident id -> fprintf ppf "%a" ident id
  | Omty_signature sg ->
      fprintf ppf "@[<hov 2>sig@ %a@;<1 -2>end@]" !out_signature sg
  | Omty_alias id -> fprintf ppf "(module %a)" ident id
and signature ppf =
  function
    [] -> ()
  | [item] -> !out_sig_item ppf item
  | Osig_typext(ext, Oext_first) :: items ->
      (* Gather together the extension constructors *)
      let rec gather_extensions acc items =
        match items with
            Osig_typext(ext, Oext_next) :: items ->
              gather_extensions
                ((ext.oext_name, ext.oext_args, ext.oext_ret_type) :: acc)
                items
          | _ -> (List.rev acc, items)
      in
      let exts, items =
        gather_extensions
          [(ext.oext_name, ext.oext_args, ext.oext_ret_type)]
          items
      in
      let te =
        { otyext_name = ext.oext_type_name;
          otyext_params = ext.oext_type_params;
          otyext_constructors = exts;
          otyext_private = ext.oext_private }
      in
        fprintf ppf "%a@ %a" type_extension te signature items
  | item :: items ->
      fprintf ppf "%a@ %a" !out_sig_item item signature items
and sig_item ppf =
  function
    Osig_class (vir_flag, name, params, clt, rs) ->
      fprintf ppf "@[<2>%s%s@ %a%s@ :@ %a@]"
        (if rs = Orec_next then "and" else "class")
        (if vir_flag then " virtual" else "") class_params params
        name class_type clt
  | Osig_class_type (vir_flag, name, params, clt, rs) ->
      fprintf ppf "@[<2>%s%s@ %a%s@ =@ %a@]"
        (if rs = Orec_next then "and" else "class type")
        (if vir_flag then " virtual" else "") class_params params
        name class_type clt
  | Osig_typext (ext, Oext_exception) ->
      fprintf ppf "@[<2>exception %a@]"
        constr (ext.oext_name, ext.oext_args, ext.oext_ret_type)
  | Osig_typext (ext, _es) ->
      extension_constructor ppf ext
  | Osig_modtype (name, Omty_abstract) ->
      fprintf ppf "@[<2>module type %s@]" name
  | Osig_modtype (name, mty) ->
      fprintf ppf "@[<2>module type %s =@ %a@]" name !out_module_type mty
  | Osig_module (name, Omty_alias id, _) ->
      fprintf ppf "@[<2>module %s =@ %a@]" name ident id
  | Osig_module (name, mty, rs) ->
      fprintf ppf "@[<2>%s %s :@ %a@]"
        (match rs with Orec_not -> "module"
                     | Orec_first -> "module rec"
                     | Orec_next -> "and")
        name !out_module_type mty
  | Osig_type(td, rs) ->
        type_decl
          (match rs with
           | Orec_not   -> "type nonrec"
           | Orec_first -> "type"
           | Orec_next  -> "and")
          ppf td
  | Osig_value vd ->
      let kwd = if vd.oval_prims = [] then "val" else "external" in
      let pr_prims ppf =
        function
          [] -> ()
        | s :: sl ->
            fprintf ppf "@ = \"%s\"" s;
            List.iter (fun s -> fprintf ppf "@ \"%s\"" s) sl
      in
      fprintf ppf "@[<2>%s %a :@ %a%a%a@]" kwd value_ident vd.oval_name
        type' vd.oval_type pr_prims vd.oval_prims
        (fun ppf -> List.iter (fun a -> fprintf ppf "@ [@@@@%s]" a.oattr_name))
        vd.oval_attributes
  | Osig_ellipsis -> ellipsis ppf

and type_decl kwd ppf td =
  let constraints ppf =
    List.iter
      (fun (ty1, ty2) ->
         fprintf ppf "@ @[<2>constraint %a =@ %a@]" type' ty1
           type' ty2)
      td.otype_cstrs
  in
  let type_defined ppf =
    match td.otype_params with
      [] -> pp_print_string ppf td.otype_name
    | [param] -> fprintf ppf "@[%a@ %s@]" type_parameter param td.otype_name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (list type_parameter (fun ppf -> fprintf ppf ",@ "))
          td.otype_params
          td.otype_name
  in
  let manifest ppf =
    function
      Otyp_manifest (ty, _) -> fprintf ppf " =@ %a" type' ty
    | _ -> ()
  in
  let name_params ppf =
    fprintf ppf "%s %t%a" kwd type_defined manifest td.otype_type
  in
  let ty =
    match td.otype_type with
      Otyp_manifest (_, ty) -> ty
    | _ -> td.otype_type
  in
  let private' ppf = function
    Asttypes.Private -> fprintf ppf " private"
  | Asttypes.Public -> ()
  in
  let immediate ppf =
    if td.otype_immediate then fprintf ppf " [%@%@immediate]" else ()
  in
  let unboxed ppf =
    if td.otype_unboxed then fprintf ppf " [%@%@unboxed]" else ()
  in
  let tkind ppf = function
  | Otyp_abstract -> ()
  | Otyp_record lbls ->
      fprintf ppf " =%a %a"
        private' td.otype_private
        record_decl lbls
  | Otyp_sum constrs ->
      fprintf ppf " =%a@;<1 2>%a"
        private' td.otype_private
        (list constr (fun ppf -> fprintf ppf "@ | ")) constrs
  | Otyp_open ->
      fprintf ppf " = ‥"
  | ty ->
      fprintf ppf " =%a@;<1 2>%a"
        private' td.otype_private
        type' ty
  in
  fprintf ppf "@[<2>@[<hov 2>%t%a@]%t%t%t@]"
    name_params
    tkind ty
    constraints
    immediate
    unboxed

and constr ppf (name, tyl,ret_type_opt) =
  let name =
    match name with
    | "::" -> "(::)"   (* #7200 *)
    | s -> s
  in
  match ret_type_opt with
  | None ->
      begin match tyl with
      | [] ->
          pp_print_string ppf name
      | _ ->
          fprintf ppf "@[<2>%s of@ %a@]" name
            (typlist simple_type " *") tyl
      end
  | Some ret_type ->
      begin match tyl with
      | [] ->
          fprintf ppf "@[<2>%s :@ %a@]" name simple_type  ret_type
      | _ ->
          fprintf ppf "@[<2>%s :@ %a ⟶ %a@]" name
            (typlist simple_type " *")
            tyl simple_type ret_type
      end

and extension_constructor ppf ext =
  let extended_type ppf =
    let type_parameter ppf ty = tyvar false ppf ty
    in
      match ext.oext_type_params with
        [] -> fprintf ppf "%s" ext.oext_type_name
      | [ty_param] ->
        fprintf ppf "@[%a@ %s@]"
          type_parameter
          ty_param
          ext.oext_type_name
      | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (list type_parameter (fun ppf -> fprintf ppf ",@ "))
          ext.oext_type_params
          ext.oext_type_name
  in
  fprintf ppf "@[<hov 2>type %t +=%s@;<1 2>%a@]"
    extended_type
    (if ext.oext_private = Asttypes.Private then " private" else "")
    constr (ext.oext_name, ext.oext_args, ext.oext_ret_type)

and type_extension ppf te =
  let extended_type ppf =
    let type_parameter ppf ty = tyvar false ppf ty in
    match te.otyext_params with
      [] -> fprintf ppf "%s" te.otyext_name
    | [param] ->
      fprintf ppf "@[%a@ %s@]"
        type_parameter param
        te.otyext_name
    | _ ->
        fprintf ppf "@[(@[%a)@]@ %s@]"
          (list type_parameter (fun ppf -> fprintf ppf ",@ "))
          te.otyext_params
          te.otyext_name
  in
  fprintf ppf "@[<hov 2>type %t +=%s@;<1 2>%a@]"
    extended_type
    (if te.otyext_private = Asttypes.Private then " private" else "")
    (list constr (fun ppf -> fprintf ppf "@ | "))
    te.otyext_constructors

let _ = out_module_type := module_type
let _ = out_signature := signature
let _ = out_sig_item := sig_item
let _ = out_type_extension := type_extension

(* Phrases *)

let exception' ppf exn outv =
  match exn with
    Sys.Break -> fprintf ppf "Interrupted.@."
  | Out_of_memory -> fprintf ppf "Out of memory during evaluation.@."
  | Stack_overflow ->
      fprintf ppf "Stack overflow during evaluation (looping recursion?).@."
  | _ -> fprintf ppf "@[Exception:@ %a.@]@." value outv

let rec items ppf =
  function
    [] -> ()
  | (Osig_typext(ext, Oext_first), None) :: xitems ->
      (* Gather together extension constructors *)
      let rec gather_extensions acc items =
        match xitems with
            (Osig_typext(ext, Oext_next), None) :: items ->
              gather_extensions
                ((ext.oext_name, ext.oext_args, ext.oext_ret_type) :: acc)
                items
          | _ -> (List.rev acc, items)
      in
      let exts, xitems =
        gather_extensions
          [(ext.oext_name, ext.oext_args, ext.oext_ret_type)]
          xitems
      in
      let te =
        { otyext_name = ext.oext_type_name;
          otyext_params = ext.oext_type_params;
          otyext_constructors = exts;
          otyext_private = ext.oext_private }
      in
        fprintf ppf "@[%a@]" type_extension te;
        if xitems <> [] then fprintf ppf "@ %a" items xitems
  | (tree, valopt) :: xitems ->
      begin match valopt with
        Some v ->
          fprintf ppf "@[<2>%a =@ %a@]" !out_sig_item tree
            value v
      | None -> fprintf ppf "@[%a@]" !out_sig_item tree
      end;
      if xitems <> [] then fprintf ppf "@ %a" items xitems

let phrase ppf =
  function
    Ophr_eval (outv, ty) ->
      fprintf ppf "@[- : %a@ =@ %a@]@." type' ty value outv
  | Ophr_signature [] -> ()
  | Ophr_signature xitems -> fprintf ppf "@[<v>%a@]@." items xitems
  | Ophr_exception (exn, outv) -> exception' ppf exn outv
