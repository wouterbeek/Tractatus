:- module(
  tree,
  [
    explicit_path/1, % ?Path
    implicit_path/1, % ?Path
    tree/1           % -Tree
  ]
).
:- reexport(parse).

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(solution_sequences)).
:- use_module(library(tree/s_tree)).





%! explicit_path(+Path) is semidet.
%! explicit_path(-Path) is multi.

explicit_path(Path) :-
  proposition(en, Path, _).



%! implicit_path(+Path) is semidet.
%! implicit_path(-Path) is multi.

implicit_path([∅|ImplicitPath]) :-
  distinct(ImplicitPath, (
    explicit_path(ExplicitPath),
    prefix(ImplicitPath, ExplicitPath)
  )).



%! tree(-Tree) is det.

tree(Tree) :-
  findall(Path, implicit_path(Path), Paths),
  paths_to_trees(Paths, Trees),
  Trees = [Tree].
