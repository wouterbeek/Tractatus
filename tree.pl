:- module(
  tree,
  [
    explicit_path/1, % ?Path
    implicit_path/1, % ?Path
    lang/1,          % ?LTag
    tree/1           % -Tree
  ]
).
:- reexport(parse, [
     proposition/3   % ?LTag, ?Path, % ?Content
   ]).

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(solution_sequences)).
:- use_module(library(tree/s_tree)).





%! explicit_path(+Path) is semidet.
%! explicit_path(-Path) is multi.

explicit_path(Path) :-
  % The language we choose here is arbitrary.
  once(lang(LTag)),
  proposition(LTag, Path, _).



%! implicit_path(+Path) is semidet.
%! implicit_path(-Path) is multi.

implicit_path([∅|ImplicitPath]) :-
  distinct(ImplicitPath, (
    explicit_path(ExplicitPath),
    prefix(ImplicitPath, ExplicitPath)
  )).



%! lang(+LTag) is semidet.
%! lang(-LTag) is multi.

lang(LTag) :-
  distinct(LTag, proposition(LTag, _, _)).



%! tree(-Tree) is det.

tree(Tree) :-
  findall(Path, implicit_path(Path), Paths),
  paths_to_trees(Paths, Trees),
  Trees = [Tree].
