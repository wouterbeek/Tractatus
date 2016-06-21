:- module(
  tractatus_export,
  [
    exp_org_mode/1, % ?LTag
    exp_simple/0,
    exp_simple/1    % ?LTag
  ]
).

/** <module> Tractatus: Export

Export the Tractatus to various serialization formats.
Currently supported:
  - Simple text-based tree
  - Org mode

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(string_ext)).
:- use_module(library(tree/s_tree)).

:- use_module(tractatus_api).





%! exp_org_mode(+LTag) is det.
%! exp_org_mode(-LTag) is multi.

exp_org_mode(LTag) :-
  lang(LTag),
  tree(Tree),
  file_name_extension(LTag, org, File),
  setup_call_cleanup(
    open(File, write, Sink),
    print_tree(Tree, [node_writer(org_writer(LTag)),output(Sink)]),
    close(Sink)
  ).



%! exp_simple is det.

exp_simple :-
  exp_simple(en).


%! exp_simple(+LTag) is det.
%! exp_simple(-LTag) is multi.

exp_simple(LTag) :-
  lang(LTag),
  tree(Tree),
  print_tree(Tree, [node_writer(simple_writer(LTag))]).





% HELPERS %

org_writer(LTag, InvPath, _) -->
  {
    reverse(InvPath, [_|Path]),
    (proposition(LTag, Path, S) -> true ; S = "∅"),
    length(InvPath, Len)
  },
  #(Len, star), " ",  index(Path), " ", str(S).


%! index(+Path:list(between(0,9)))// is det.
% Writes Path as Tractarian index.

index([]) --> !, "".
index([H|T]) --> integer(H), ({T == []} -> "" ; ".", '*'(integer, T)).


%! star// is det.
% Org mode uses stars to shape a tree.

star --> "*".



simple_writer(LTag, [Node|InvPath], _) -->
  {
    reverse([Node|InvPath], [_|Path]),
    (proposition(LTag, Path, S) -> true ; S = "∅"),
    string_truncate(S, 40, TruncatedS)
  },
  dcg_tree:indent_path(InvPath),
  atom(Node), " \t", str(TruncatedS).
