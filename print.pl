:- module(
  print,
  [
    org_mode_export/1, % +Lang
    print_tree/0,
    print_tree/1       % ?Lang
  ]
).

/** <module> Tractatus: print

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_tree)).
:- use_module(library(string_ext)).

:- use_module(tree).





%! org_mode_export(+Lang) is det.

org_mode_export(Lang) :-
  tree(Tree),
  file_name_extension(Lang, org, File),
  setup_call_cleanup(
    open(File, write, Sink),
    print_tree0(Sink, Tree, org_writer(Lang)),
    close(Sink)
  ).



%! print_tree is det.

print_tree :-
  print_tree(en).


%! print_tree(+Lang) is det.
%! print_tree(-Lang) is multi.

print_tree(Lang) :-
  tree(Tree),
  print_tree0(Tree, node_writer(Lang)).



% HELPERS %

print_tree0(Tree, NodeWriter) :-
  print_tree0(current_output, Tree, NodeWriter).

print_tree0(Sink, Tree, NodeWriter) :-
  dcg_with_output_to(Sink,
    dcg_tree(Tree, [node_writer(NodeWriter)])
  ).

org_writer(Lang, [Node|InvPath], _) -->
  {
    reverse([Node|InvPath], [_|Path]),
    (proposition(Lang, Path, S) -> true ; S = "∅")
  },
  stars([Node|InvPath]), " ",  path(Path), " ", str(S).

path([]) --> !, "".
path([H|T]) --> integer(H), ({T == []} -> "" ; ".", integers(T)).

integers([]) --> !, "".
integers([H|T]) --> integer(H), integers(T).

stars([]) --> !, "".
stars([_|T]) --> "*", stars(T).

tree_writer(Lang, [Node|InvPath], _) -->
  {
    reverse([Node|InvPath], [_|Path]),
    (proposition(Lang, Path, S) -> true ; S = "∅"),
    string_truncate(S, 40, TruncatedS)
  },
  dcg_tree:indent_path(InvPath),
  atom(Node), " \t", str(TruncatedS).
