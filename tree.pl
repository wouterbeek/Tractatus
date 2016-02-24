:- module(
  tree,
  [
    explicit_path/1, % ?Path
    implicit_path/1, % ?Path
    tree/0,
    tree/1           % +Lang
  ]
).

:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_tree)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(solution_sequences)).
:- use_module(library(string_ext)).
:- use_module(library(tree/s_tree)).

:- use_module(parse).



explicit_path(Path) :-
  proposition(en, Path, _).


implicit_path([∅|ImplicitPath]) :-
  distinct(ImplicitPath, (
    explicit_path(ExplicitPath),
    prefix(ImplicitPath, ExplicitPath)
  )).


tree :-
  tree(en).

tree(Lang) :-
  findall(Path, implicit_path(Path), Paths),
  paths_to_trees(Paths, Trees),
  Trees = [Tree],
  dcg_with_output_to(current_output, dcg_tree(node(Lang), Tree)).

node(Lang, Node, InvPath, _) -->
  {
    reverse([Node|InvPath], [_|Path]),
    (proposition(Lang, Path, S) -> true ; S = "∅"),
    string_truncate(S, 40, TruncatedS)
  },
  atom(Node), " \t", str(TruncatedS).
