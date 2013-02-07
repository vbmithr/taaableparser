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

let tree_of_triple (n1, a, n2) = Node (n2, [Leaf n1, a])

(* Like List.map but for a tree *)
let rec tree_map f = function
  | Leaf l -> f (Leaf l)
  | Node (n, ns) -> f (Node (n, List.map (fun (t,a) -> (f t), a) ns))

(* Like List.mem but for a tree *)
let rec tree_mem n = function
  | Leaf l -> n = l
  | Node (l, ns) -> l = n || List.exists (fun (t,a) -> (tree_mem n t)) ns

(* Try to merge tr1 and tr2 *)
let merge_trees tr1 tr2 =
  let merged = ref false in
  let map_fun target n = match n, target with
    | Node (l, ns), Node (ll, nss) ->
      if l = ll then (merged := true; Node (l, ns @ nss)) else Node (l, ns)
    | _ -> failwith "merge_trees"
  in
  let new_tree = tree_map (map_fun tr2) tr1 in
  if not !merged then
    let new_tree = tree_map (map_fun tr1) tr2 in
    if !merged then [new_tree] else [tr1; tr2]
  else [new_tree]

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
  Printf.sprintf "%s(%s)" n.name ingredients

let string_of_triple (n1, action, n2) =
  Printf.sprintf "%s: %s => %s"
    (string_of_action action) (string_of_node n1) (string_of_node n2)
