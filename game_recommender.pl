% Marcus, Alex, Felix

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- dynamic(found/2).

% Make a call to the Game API and build the knowledge base dynamically
steam(Dict, R, S) :-
  setup_call_cleanup(
    http_open('https://api.myjson.com/bins/aj8tz', In, []),
    json_read_dict(In, Dict),
    close(In)
  ),
  dict_x(Dict, R),
  dict_y(R,S).

% Parse the JSON file retrieved from the API call and build knowledge base dynamically
dict_x(X, X.apps).
dict_y([H|R], H) :-
  % Remove the quotations from the JSON values
  term_to_atom(Name,H.name),
  term_to_atom(Genre,H.genre),
  term_to_atom(Rating,H.rating),
  term_to_atom(Graphics,H.graphics),
  term_to_atom(ReleaseYear,H.releaseyear),
  % Build knowledge base
  assert(game(Name)),
  assert(game_genre(Name,Genre)),
  assert(game_rating(Name,Rating)),
  assert(game_graphics(Name,Graphics)),
  assert(game_releaseyear(Name,ReleaseYear)),
  dict_y(R, _).

% Build the knowledge base first whenever the project is made before allowing
% users to make queries
%% :- initialization(steam(_,_,_)).

% Natural language processing. Code is based off of David Poole's solution to
% Question 3 in Assignment 4 regarding geography
noun_phrase(T0,T4,Obj,C0,C4) :-
    det(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2),
    noun(T2,T3,Obj,C2,C3),
    mp(T3,T4,Obj,C3,C4).

% Determiners.
det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det([as | T],T,_,C,C).
det(T,T,_,C,C).

% Adjectives.
adjectives(T,T,_,C,C).
adjectives(T0,T2,Obj,C0,C2) :-
    adjective(T0,T1,Obj,C0,C1),
    adjectives(T1,T2,Obj,C1,C2).

adjective([H,s | T],T,Obj,C,[language(Obj,H)|C]).

% Modifying phrases
mp(T,T,_,C,C).
mp(T0,T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).
mp([that|T0],T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).
mp([that,is|T0],T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).
mp([with|T0],T2,O1,C0,C2) :-
    reln(T0,T1,O1,O2,C0,C1),
    noun_phrase(T1,T2,O2,C1,C2).
  mp([with, the|T0],T2,O1,C0,C2) :-
      reln(T0,T1,O1,O2,C0,C1),
      noun_phrase(T1,T2,O2,C1,C2).

% Noun
noun([game | T],T,Obj,C,[game(Obj)|C]).
noun([X | T],T,X,C,C) :- game(X).

% Relations.
reln([similar, to| T],T,O1,O2,_,[similarGenre(O1,O2)]).
reln([similar, genre, as| T],T,O1,O2,_,[similarGenre(O1,O2)]).
reln([same, genre | T],T,O1,O2,_,[similarGenre(O1,O2)]).
reln([similar, ratings, as| T],T,O1,O2,_,[similarRating(O1,O2)]).
reln([similar, graphics, as| T],T,O1,O2,_,[similarGraphics(O1,O2)]).
reln([same, ratings, as| T],T,O1,O2,_,[similarRating(O1,O2)]).
reln([same, graphics, as| T],T,O1,O2,_,[similarGraphics(O1,O2)]).
reln([same, release, year, as| T],T,O1,O2,_,[similarReleaseYear(O1,O2)]).
reln([not, similar, to| T],T,O1,O2,_,[notSimilarGenre(O1,O2)]).

% Questions.
question([is | T0],T2,Obj,C0,C2) :-
    noun_phrase(T0,T1,Obj,C0,C1),
    mp(T1,T2,Obj,C1,C2).
question([what,is | T0],T1,Obj,C0,C1) :-
    mp(T0,T1,Obj,C0,C1).
question([what,is | T0],T1,Obj,C0,C1) :-
    noun_phrase(T0,T1,Obj,C0,C1).
question([what | T0],T2,Obj,C0,C2) :-
    noun_phrase(T0,T1,Obj,C0,C1),
    mp(T1,T2,Obj,C1,C2).
question([give,me | T0],T1,Obj,C0,C1) :-
    mp(T0,T1,Obj,C0,C1).
question([give,me | T0],T1,Obj,C0,C1) :-
    noun_phrase(T0,T1,Obj,C0,C1).
question([recommend | T0],T1,Obj,C0,C1) :-
    mp(T0,T1,Obj,C0,C1).
question([recommend | T0],T1,Obj,C0,C1) :-
    noun_phrase(T0,T1,Obj,C0,C1).

% Gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A,[],C),
    prove_all(C).

% Proves all elements of L against the database
prove_all([]).
prove_all([H|T]) :-
    call(H),
    prove_all(T).

% Helper functions to complete specific queries
% Retrieve games with a similar genre
similarGenre(X,Y) :-
  setof((X,Y), B^(game_genre(X,B), game_genre(Y,B), \+X=Y), Game),
  member((X,Y), Game).

% Retrieve games with a different genre
notSimilarGenre(X,Y) :-
  X \= Y,
  game_genre(X,B),
  game_genre(Y,A),
  B \= A.

% Retreive games with a similar rating
similarRating(X,Y) :-
  setof((X,Y), B^(game_rating(X,B), game_rating(Y,B), \+X=Y), Game),
  member((X,Y), Game).

% Retrieve games with similar graphics
similarGraphics(X,Y) :-
  setof((X,Y), B^(game_graphics(X,B), game_graphics(Y,B), \+X=Y), Game),
  member((X,Y), Game).

% Retrieve games with a similar release year
similarReleaseYear(X,Y) :-
  setof((X,Y), B^(game_releaseyear(X,B), game_releaseyear(Y,B), \+X=Y), Game),
  member((X,Y), Game).

% Prompts the user to put in a query
q(Ans) :-
    write("Ask Game Bot: "),flush_output(current_output),
    readln(Ln),
    question(Ln,End,Ans,[],C),
    prove_all(C),
    member(End,[[],['?'],['.']]).
