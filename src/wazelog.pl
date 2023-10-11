:- module(wazelog, [start/1]).
:- use_module(library(readutil)).
:- use_module(nlp).
:- use_module(lang).
:- use_module(path).
:- use_module(routes).


% ---- User Interface ----
wazelog_writeln(Msg) :-
	format(" WazeGPT : ~s 	\n", [Msg]).

% Add spacing between outputs
spacing :-
	tty_size(_, Width),
	string_repeat("=", Width, Repeated),
	writeln(Repeated).


string_repeat(_, 0, "") :-
	!.
string_repeat(Base, Times, Repeated) :-
	Pred is Times - 1,
	string_repeat(Base, Pred, Next),
	string_concat(Base, Next, Repeated).



% ---- Read Useer Input ---- 


read_user_input(Result) :-
	user_title(Title),
	format("~s: ", [Title]),
	current_input(Stdin),
	read_string(Stdin, "\n", "\r\t ", _, Text), 
	parse_user_input(Text, ParseResult),
	(
		ParseResult = ok(Input),
		not(stop(Input)),
		!,
		ParseResult = Result;

		ParseResult \= ok(_),
		!,
		ParseResult = Result;

		Result = bye
	).


% Rule: decorate_input(Path, S, SRes).
% Format list to output string
decorate_input(Path, SRes) :-
	decorate_input(Path, [], SRes).
decorate_input([], S, SRes) :-
	reverse(S, Inv),
	atomics_to_string(Inv, ", ", SRes).
decorate_input([City | Path], S, SRes) :-
	city(City, Trad),
	decorate_input(Path, [Trad | S], SRes).


start(Then) :-
	run(start, _, Then),
	!.



run(bye, _, stop) :-
	farewell(Text),
	wazelog_writeln(Text).
run(start, _, Then) :-
	ask_in_loop(ask_city(q_src), Src),
	run(Src, start, Then).
run(city(Src), start, Then) :-
	ask_in_loop(ask_city(q_dest), Dest),
	run(Dest, src(Src), Then).
run(city(Dest), src(Src), Then) :-
	ask_in_loop(ask_stops, Paradas),
	run(Paradas, src_dest(Src, Dest), Then).
run(stops(Paradas), src_dest(Src, Dest), continue) :-
	shortest_path_through(Src, Paradas, Dest, Result),
	spacing,
	(
		Result = shortest_path(Ruta, Cost),
		decorate_input(Ruta, StrPath),
		display_path(StrPath, Cost, DisplayPath),
		wazelog_writeln(DisplayPath);

		Result = no_route(From, To),
		city(From, StrFrom),
		city(To, StrTo),
		display_no_route(StrFrom, StrTo, DisplayNoRoute),
		wazelog_writeln(DisplayNoRoute)
	),
	run(bye, _, _),
	spacing.


% Rule: ask_in_loop(Predicate, Input).
% Ask for user input until the user says bye.
ask_in_loop(Predicate, Input) :-
	ask_in_loop(first, Predicate, Input).
ask_in_loop(Iteration, Predicate, Input) :-
	call(Predicate, repeat(Iteration, Then)),
	!,
	(
		Then = done(Input),
		!;

		ask_in_loop(Then, Predicate, Input)
	).
ask_in_loop(again(Iteration), Predicate, Input) :-
	!,
	ask_in_loop(again(Iteration), Predicate, Input).
ask_in_loop(Iteration, Predicate, Input) :-
	ask_in_loop(again(Iteration), Predicate, Input).


% Rule: ask_city(Prompter, repeat(Iteration, done(Out))).
% Ask for a city if the user input is a city type.
ask_city(Prompter, repeat(Iteration, done(Out))) :-
	call(Prompter, Iteration, Prompt),
	wazelog_writeln(Prompt),
	read_user_input(Input),
	(
		Input = bye,
		Out = bye;

		Input = ok(CityRaw),
		key_nominal(CityRaw, Nominal),
		pinpoint(Nominal, City),
		Out = city(City)
	).



% Rule: ask_stops(repeat(Iteration, Then)).
% Ask for stops if the user input is a stop type.
% ---- Ask for stops ----
ask_stops(repeat(Iteration, Then)) :-
	q_stops(Iteration, Prompt),
	wazelog_writeln(Prompt),
	last_stops(Iteration, Stops),
	read_user_input(Result),
	(
		Result = bye,
		Then = done(bye);

		Result = ok(Input),
		(
			key_nominal(Input, Nominal),
			!,
			not(stop_asking_intermed(Input)),
			pinpoint(Nominal, Stop),
			append(Stops, [Stop], NextStops),
			Then = stops(NextStops);

			stop_asking_intermed(Input),
			Then = done(stops(Stops))
		)
	).


% Rule: last_stops(Iteration, Stops).
% Extract the list of stops from an iteration.
last_stops(first, []).
last_stops(stops(Stops), Stops).
last_stops(again(Iteration), Stops) :-
	last_stops(Iteration, Stops).



% Rule: pinpoint(nominal(Place, Orig, Bare), Stop).
% Ask for a place if the user input is a place type.
pinpoint(nominal(Stop, _, _), Stop) :-
	city(Stop, _),
	!.
pinpoint(nominal(Place, Orig, Bare), Stop) :-
	(
		place_type(Place),
		!,
		q_which(Bare, Prompt);

		q_direction(Orig, Prompt)
	),
	wazelog_writeln(Prompt),
	read_user_input(ok(Input)),
	key_nominal(Input, Nominal),
	pinpoint(Nominal, Stop).


% Rule: stop_asking_intermed(Input).
% Stop if the app receives a negative exclamation.
% ---- Stop asking for stops ----
stop_asking_intermed(Input) :- 
	contains_term(exclamation(negative), Input).


% Rule: stop(Input).
% Stop if the app receives a bye.
stop(Input) :- 
	contains_term(exclamation(bye), Input).
