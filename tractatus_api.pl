:- module(
  tractatus_api,
  [
    explicit_path/1, % ?Path
    implicit_path/1, % ?Path
    lang/1,          % ?LTag
    proposition/3,   % ?LTag, ?Path, ?Content
    tree/1           % -Tree
  ]
).

/** <module> Tractatus: API

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(apply)).
:- use_module(library(char_ext)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(persistency)).
:- use_module(library(solution_sequences)).
:- use_module(library(tree/s_tree)).

:- initialization(db_attach('tractatus.db', [sync(flush)])).

%! proposition(
%!   ?LTag:oneof([de,en]),
%!   ?Path:list(between(0,9)),
%!   ?Proposition:list(code)
%! ) is nondet.

:- persistent proposition(oneof([de,en]), list(between(0,9)), list).





%! explicit_path(+Path) is semidet.
%! explicit_path(-Path) is multi.
% Explicit paths are paths for which a proposition appears in the Tractatus.

explicit_path(Path) :-
  % The language we choose here is arbitrary.
  once(lang(LTag)),
  proposition(LTag, Path, _).



%! implicit_path(+Path) is semidet.
%! implicit_path(-Path) is multi.
% An implicit path is a path for which no proposition appears in the Tractatus.
% The presence of an implicit path is indicated by the explicit paths, in
% order to result in a tree structure.

implicit_path([∅|ImplicitPath]) :-
  distinct(ImplicitPath, (
    explicit_path(ExplicitPath),
    prefix(ImplicitPath, ExplicitPath)
  )).



%! index_path(+Index:atom, +Path:list(between(0,9))) is semidet.
%! index_path(+Index:atom, -Path:list(between(0,9))) is det.
%! index_path(-Index:atom, +Path:list(between(0,9))) is det.
% Translates between indices and paths.

index_path(Index, Path) :-
  atom(Index), !,
  remove_dot0(Index, WithoutDot),
  atom_chars(WithoutDot, Chars),
  maplist(char_digit, Chars, Path).
index_path(Index, [H]) :- !,
  char_digit(Index, H).
index_path(Index, [H|T]) :-
  is_list([H|T]), !,
  atomic_list_concat([H,.|T], Index).

remove_dot0(WithDot, WithoutDot) :-
  atomic_list_concat(L, ., WithDot),
  atomic_list_concat(L, WithoutDot).



%! lang(+LTag) is semidet.
%! lang(-LTag) is multi.
% The languages that are supported by the Tractatus API.

lang(LTag) :-
  distinct(LTag, proposition(LTag, _, _)).



%! tree(-Tree) is det.
% Returns the full tree structure of the Tractatus.

tree(Tree) :-
  findall(Path, implicit_path(Path), Paths),
  paths_to_trees(Paths, Trees),
  Trees = [Tree].
