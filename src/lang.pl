:- module(lang, [sentence_sep/1, exclamation/2, verbal/1, before_nominal/1,
                 contraction/2, unclassified/1, place_type/1, set_lang/1,
				 q_src/2, q_dest/2, q_direction/2, q_which/2, q_stops/2,
				 farewell/1, user_title/1, display_path/3, display_no_route/3]).


supported_lang(es).


% Built in lang function to get the current language
plang(es).
:- dynamic lang/1.


% Rule: set_lang(language)
% Description: Changes the language in use. The language
% entered must be supported according to the documentation
set_lang(Lang) :-
	supported_lang(Lang),
	retractall(lang(_)),
	asserta(lang(Lang)).


% Facts: sentence_sep(token).
% Description: Enumerates the tokens that separate sentences.
sentence_sep('.').
sentence_sep(',').
sentence_sep(';').
sentence_sep(':').
sentence_sep('!').


% Rule: place_type(place_type)
% Description: Enumerates the nominal forms that are known
% to refer to place types instead of specific places. This
% rule succeeds if there is a corresponding fact for the
% language.
place_type(PlaceType) :-
	lang(Lang),
	place_type(Lang, PlaceType).
place_type(es, supermercado).
place_type(es, mercado).
place_type(es, tienda).
place_type(es, banco).
place_type(es, hospital).
place_type(es, escuela).
place_type(es, restaurante).
place_type(es, parque).
place_type(es, instituto).
place_type(en, supermercado).
place_type(en, market).
place_type(en, store).
place_type(en, bank).
place_type(en, hospital).
place_type(en, school).
place_type(en, restaurant).
place_type(en, park).
place_type(en, institute).




% Rule: exclamation(token, type)
% Description: Relates keywords of exclamative sentences
% with the internal type of exclamation. At the same time,
% this rule identifies the fact that such keywords conform
% this type of sentences.
exclamation(Exclamation, Type) :-
	lang(Lang),
	exclamation(Lang, Exclamation, Type).
exclamation(es, si, affirmative).
exclamation(es, no, negative).
exclamation(es, hola, greeting).
exclamation(es, adios, bye).
exclamation(es, chao, bye).
exclamation(es, gracias, misc).
exclamation(en, yes, affirmative).
exclamation(en, no, negative).
exclamation(en, hi, greeting).
exclamation(en, hello, greeting).
exclamation(en, bye, bye).
exclamation(en, goodbye, bye).
exclamation(en, thank, misc).
exclamation(en, thanks, misc).


% Rule: verbal(token)
% Description: Succeeds if the token is a verbal form.
verbal(Verb) :-
	lang(Lang),
	verbal(Lang, Verb).
verbal(es, esta).
verbal(es, estoy).
verbal(es, encuentro).
verbal(es, encuentra).
verbal(es, voy).
verbal(es, necesito).
verbal(es, requiero).
verbal(es, ir).
verbal(es, es).
verbal(es, tengo).
verbal(es, llegar).
verbal(es, pasar).
verbal(es, ubica).
verbal(es, gustaria).
verbal(es, quisiera).
verbal(en, is).
verbal(en, am).
verbal(en, have).
verbal(en, go).
verbal(en, going).
verbal(en, need).
verbal(en, needs).
verbal(en, arrive).
verbal(en, located).
verbal(en, like).



% Rule: before_nominal(token).
% Description: Enumerates filler words that are expected
% to be followed immediately by a nominal form. This
% includes in the case of Spanish the articles and some
% prepositions. This allows to include these words in
% the original articulated form of the nominal term
% without affecting its identifying atom with respect
% to not having written the preceding word.

before_nominal(Word) :-
	lang(Lang),
	before_nominal(Lang, Word).
before_nominal(es, el).
before_nominal(es, los).
before_nominal(es, la).
before_nominal(es, las).
before_nominal(es, de).
before_nominal(en, the).
before_nominal(en, of).


% Rule: contraction(token, Expansion).
% Description: Expands contractions into equivalent forms.
contraction(Word, Expanded) :-
	lang(Lang),
	contraction(Lang, Word, Expanded).
contraction(es, al, [a, el]).
contraction(es, del, [de, el]).
contraction(en, 'i\'m', [i, am]).


% Rule: unclassified(token).
% Description: Declares that a word is explicitly unclassified
% and therefore should be considered as filler (see `filler/1`)
% instead of nominal, since the latter is assumed by default.
unclassified(Word) :-
	lang(Lang),
	unclassified(Lang, Word).
unclassified(es, me).
unclassified(es, que).
unclassified(es, a).
unclassified(es, se).
unclassified(es, en).
unclassified(es, de).
unclassified(es, un).
unclassified(es, una).
unclassified(es, por).
unclassified(es, muchas).
unclassified(en, a).
unclassified(en, an).
unclassified(en, at).
unclassified(en, in).
unclassified(en, to).
unclassified(en, by).
unclassified(en, many).
unclassified(en, lot).
unclassified(en, lots).



% Rule; q_src(iteration, Prompt).
% Description: Defines the source question. This question
% may change if an incorrect answer is given the first
% time, which is expressed in the iteration parameter.
q_src(Iteration, Prompt) :-
	lang(Lang),
	q_src(Lang, Iteration, Prompt).
q_src(es, first, "Bienvenido a WazeLog.  Donde se encuentra?").
q_src(es, again(_), "No entiendo tu respuesta. Cual es su ubicacion actual?").

q_src(en, first, "Welcome to WazeLog, the best logic to arrive at your destination. Where are you?").
q_src(en, again(_), "Sorry, I could not understand you. Where are you right now?").


% Rule: q_dest(iteration, Prompt).
% Description: Defines the destination question. This question
% may change if an incorrect answer is given the first
% time, which is expressed in the iteration parameter.
q_dest(Iteration, Prompt) :-
	lang(Lang),
	q_dest(Lang, Iteration, Prompt).
q_dest(es, first, "Super, cual es su destino?").
q_dest(es, again(_), "Dado mi conocimiento actual, no puedo ofrecer una respuesta precisa a esa pregunta. A donde te dirige? ").
q_dest(en, first, "Great! Where are heading to?").
q_dest(en, again(_), "Given my current knowledge, I cannot provide an accurate answer to that question. What is your destination?").


% Rule: q_direction(place, Prompt).
% Description: Defines the question to ask for the location
% of a place.
% Example: q_direction("supermercado", Prompt).
%          Prompt = "Donde queda el supermercado?".
q_direction(Place, Prompt) :-
	lang(Lang),
	q_direction(Lang, Place, Prompt).
q_direction(es, Place, Prompt) :-
	format(string(Prompt), "Donde se encuentra ~w?", [Place]).
q_direction(en, Place, Prompt) :-
	format(string(Prompt), "Where is ~w located?", [Place]).


% Rule: q_which(place, Prompt).
% Description: Defines the question to ask for a specific
% place of a given type.
q_which(Place, Prompt) :-
	lang(Lang),
	q_which(Lang, Place, Prompt).
q_which(es, Place, Prompt) :-
	format(string(Prompt), "Cual ~w?", [Place]).
q_which(en, Place, Prompt) :-
	format(string(Prompt), "Which ~w?", [Place]).


% Rule: q_stops(iteration, Prompt).
% Description: Defines the questions that ask for the
% first and the following stops.
%  Prompt = "Genial, algun destino intermedio?".
q_stops(Iteration, Prompt) :-
	lang(Lang),
	q_stops(Lang, Iteration, Prompt).
q_stops(es, first, "Genial, algun destino intermedio?").
q_stops(es, stops(_), "Algun otro destino intermedio?").
q_stops(es, again(first), "Perdon, no he podido entenderle. Desea un destino intermedio?").
q_stops(es, again(stops(_)), "Perdon, no he podido entenderle. Desea otro destino intermedio?").
q_stops(en, first, "Great, is there some stop in between?").
q_stops(en, stops(_), "Any other stop?").
q_stops(en, again(first), "Again, would you like to stop midway?").
q_stops(en, again(stops(_)), "Again, would you like to stop another time?").


% Rule: farewell(Salida).
% Descripcion: Define out message
farewell(Farewell) :-
	lang(Lang),
	farewell(Lang, Farewell).
farewell(es, "Muchas gracias por utilizar WazeLog!").
farewell(en, "Thank you for using WazeLog!").


user_title(Title) :-
	lang(Lang),
	user_title(Lang, Title).
user_title(es, "Usuario").
user_title(en, "User").


% ---- Display Ruta ----
% Rule: display_path(path, cost, Text).
% Description: Defines the text to display when a path is found.
display_path(Path, Cost, Text) :-
	lang(Lang),
	display_path(Lang, Path, Cost, Text).
display_path(es, Path, cost(Cost, Min, Max), Text) :-
	format(string(Text), "Su ruta seria ~s. Longitud estimada de ~d km. Duracion ~d-~d min.", [Path, Cost, Min, Max]).
display_path(en, Path, cost(Cost, Min, Max), Text) :-
	format(string(Text), "This is your ~d-km path: ~s. It will take ~d to ~d minutes.", [Cost, Path, Min, Max]).


% ---- No path found ----
% Rule: display_no_route(From, To, Text).
% Description: Defines the text to display when no path is found.
display_no_route(From, To, Text) :-
	lang(Lang),
	display_no_route(Lang, From, To, Text).
display_no_route(es, From, To, Text) :-
	format(string(Text), "No hay una ruta conocida de ~s a ~s.", [From, To]).
display_no_route(en, From, To, Text) :-
	format(string(Text), "There's no path from ~s to ~s.", [From, To]).
