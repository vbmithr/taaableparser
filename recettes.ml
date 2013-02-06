(* Verbs representing an action performed while cooking (strings for now) *)
type verb = string

(* Adding a notion of time to a verb, allowing to time the actions *)
type action = verb * int

(* Ingredients are represented as strings for now *)
type ingredient = string

(* A node represent the state of a receipe at a point in time *)
type node =
    {
      name: string;
      step_id: int;
      ingredients: ingredient list
    }

(* A triple is a step (or transformation) of the receipe *)
type triple = node * action * node

(* A receipe is a list of triples *)
type receipe = triple list

(* A part of a receipe as seen as a tree *)
type tree = Leaf of node | Node of node * (tree * action) list

(* A receipe can be seen as a forest *)
type receipe_forest = tree list

(* Get leaves of a tree *)
let rec leaves_of_tree = function
  | Leaf l       -> [l]
  | Node (n, ns) -> List.flatten (List.map (fun (n, a) -> leaves_of_tree n) ns)

(* let tree_of_triple tr1 tr2 = *)
(*   if tr1 = tr2 then *)
(*     match tr1 with -> *)
(*       n1, a, n2 -> Node (n1, [n2, a]) *)
(*   else *)
(*     match tr1, tr2 with *)
(*       | (n11, a1, n12), (n21, a2, n22) -> *)
(*         ( *)
(*           if n12 = n22 then Node(n12, [n11, a1; n21, a2]) *)
(*           else if n12 = n21 then Node(n22, *)

(*         ) *)
(*       | _ -> failwith "unify_triple" *)


(* Misc. *)

let string_concat c = function
  | [] -> ""
  | [s] -> s
  | ss ->
  let res =
    List.fold_left (fun acc s -> acc ^ s ^ c) "" ss in
  let len = String.length res in
  String.sub res 0 (len-1)

let id_step_of_id str =
  let re = Str.regexp "_" in
  match List.rev (Str.split re str) with
    | [] -> failwith "id_step_of_id"
    | [e] -> e, 0
    | h::t -> string_concat "_" (List.rev t), (try (int_of_string h) with Failure _ -> 0)

(* Printers *)

let string_of_action (v, t) =
  if t = 0 then v else v ^ "_" ^ (string_of_int t)

let string_of_node n =
  let ingredients = string_concat "," n.ingredients in
  Printf.sprintf "%s_%d(%s)" n.name n.step_id ingredients

let string_of_triple (n1, action, n2) =
  Printf.sprintf "%s: %s => %s"
    (string_of_action action) (string_of_node n1) (string_of_node n2)
