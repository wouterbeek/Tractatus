 :- module(
  tractatus_scrape,
  [
    assert_tractatus/1, % +Url
    url/1               % -Url
  ]
    ).

/** <module> Tractatus: Scrape

Scrapes the various online Tractatus sources.  These are highly incomplete
and therefore cannot be used to base the Tractatus API on.  (See
tractatus_parse.pl for a more reliable way of obtaining the Tractatus data
structure.)  However, this module can be used to assess the incompleteness
of the online sources.

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(hash_ext)).
:- use_module(library(http/http_download)).
:- use_module(library(xpath)).



assert_tractatus(Url) :-
  url(N, Url),
  html_download(Url, Dom),
  findall(proposition(Path,Content), content0(N, Dom, Path, Content), L),
  md5(Url, Md5),
  file_name_extension(Md5, db, File),
  setup_call_cleanup(
    open(File, write, Sink),
    forall(member(X, L), writeln(Sink, X)),
    close(Sink)
  ).

% Site-specific: extract a Tractatus proposition from the HTML DOM.
content0(1, Dom, Path, Content) :-
  xpath(Dom, //li, Li),
  xpath_chk(Li, /self(@'data-name'), DottyIndex),
  dotty_index0(Index, DottyIndex),
  index_path(Index, Path),
  xpath_chk(Li, //span(@class=content), Span),
  xpath_chk(Span, /self(normalize_space), Content).
content(2, Dom, Path, Content) :-
  xpath(Dom, //dt/a(normalize_space), Index),
  index_path(Index, Path).

%! dotty_index0(+Index, -DottyIndex) is det.
% An online sources uses an extra dot for single-digit indices.

dotty_index0(Index, DottyIndex) :-
  atom_length(Index, 1), !,
  atomic_concat(Index, ., DottyIndex).
dotty_index0(Index, Index).



url(Url) :-
  url(_, Url).

url(1, 'http://www.tractatuslogico-philosophicus.com').
url(2, 'http://tractatus-online.appspot.com/Tractatus/jonathan/D.html').
