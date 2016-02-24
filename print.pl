:- module(
  print,
  [
    print_tree/0,
    print_tree/1  % ?Lang
  ]
).

/** <module> Tractatus: print

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_tree)).
:- use_module(library(string_ext)).

:- use_module(tree).



%! print_tree is det.

print_tree :-
  print_tree(en).


%! print_tree(+Lang) is det.
%! print_tree(-Lang) is multi.

print_tree(Lang) :-
  tree(Tree),
  dcg_with_output_to(current_output,
    dcg_tree(Tree, [node_writer(print_node(Lang))])
  ).


%! print_node(+Lang, +InvPath, +IsLeaf)// is det.

print_node(Lang, [Node|InvPath], _) -->
  {
    reverse([Node|InvPath], [_|Path]),
    (proposition(Lang, Path, S) -> true ; S = "∅"),
    string_truncate(S, 40, TruncatedS)
  },
  dcg_tree:indent_path(InvPath),
  atom(Node), " \t", str(TruncatedS).
