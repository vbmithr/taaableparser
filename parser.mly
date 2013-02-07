%{
  open Recettes
%}

%token<string> ID
%token COLON COMMA LPAR RPAR TO EOF

%start<Recettes.receipe> receipe

%%

receipe:
| ts = triples+ EOF { List.flatten ts }

triples:
| ac = ID COLON n1 = node+ TO n2 = node
    {
      let verb, step = id_step_of_id ac in
      List.map (fun n -> n, (verb, step), n2) n1
    }

node:
| ac = ID LPAR ingr = ingredients RPAR
    { { name=ac; ingredients=ingr } }

ingredients:
| { [] }
| cts = ingredients;  COMMA?;  ct = ID { ct :: cts }
