:- module(
  parse,
  [
    parse/0,
    proposition/3 % ?LTag, ?Path, % ?Content
  ]
).

:- use_module(library(apply)).
:- use_module(library(char_ext)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(debug)).
:- use_module(library(persistency)).
:- use_module(library(pure_input)).

:- initialization(db_attach('tractatus.db', [sync(flush)])).

:- persistent proposition(oneof([de,en]), list(between(0,9)), list).

parse :-
  absolute_file_name('source/5740-t/5740-t.tex', File, [access(read)]),
  phrase_from_file(tractatus, File).

tractatus -->
  ..., propositions(en),
  ..., propositions(de),
  ..., eos, !.
  
propositions(LTag) -->
  "\\Proposition", lang(LTag), "{", index(Path), "}\r\n{",
  ...(Cs), eop(Last),
  {
    store_proposition(LTag, Path, Cs),
    string_codes(S, Cs),
    debug(tractatus(parse), "Assert proposition ~w ~w", [Path,S])
  },
  ({Last == true} -> "" ; propositions(LTag)).

lang(en) --> "E".
lang(de) --> "G".

index(Path) -->
  digit(H),
  ("." -> digits(T) ; {T = []}),
  {maplist(char_digit, [H|T], Path)}.

eop(Last) -->
  "}", latex_eol,
  (   "\\end{propositions}"
  ->  {Last = true}
  ;   latex_eol, latex_eol, {Last = false}
  ).

latex_eol --> "%", !, ..., dos_eol.
latex_eol --> dos_eol.

dos_eol --> "\r\n".

store_proposition(LTag, Path, Cs1) :-
  phrase(normalize, Cs1, Cs2),
  string_codes(S, Cs2),
  assert_proposition(LTag, Path, S).

% Normalization removes line comments and (DOS) end of line sequenes.
normalize --> eos, !.
normalize, " " --> latex_eol, !, normalize.
normalize, [C] --> [C], normalize.
