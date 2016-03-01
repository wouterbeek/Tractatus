:- module(
  tractatus_parse,
  [
    parse/0,
    proposition/3 % ?LTag, ?Path, % ?Content
  ]
).

/** <module> Tracatatus: Parse

Parses a TeX file containing a German and an English version of the Tracatus.
Both versions are store as proposition/3 compound terms.

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(pure_input)).

:- use_module(tractatus_api).

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

index([H|T]) -->
  digit(H),
  ("." -> '*'(digit, T) ; {T = []}).

eop(Last) -->
  "}", latex_eol,
  (   "\\end{propositions}"
  ->  {Last = true}
  ;   latex_eol, latex_eol, {Last = false}
  ).

latex_eol --> "%", !, ..., dos_eol.
latex_eol --> dos_eol.

dos_eol --> "\r\n".

% Normalization removes line comments and (DOS) end of line sequenes.
normalize --> eos, !.
normalize, " " --> latex_eol, !, normalize.
normalize --> "\\-", !, normalize.
normalize, [C] --> [C], normalize.

store_proposition(LTag, Path, Cs1) :-
  phrase(normalize, Cs1, Cs2),
  string_codes(S, Cs2),
  tractatus_api:assert_proposition(LTag, Path, S).
