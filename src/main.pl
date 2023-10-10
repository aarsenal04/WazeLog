#!/usr/bin/env swipl

:- use_module(wazelog).
:- use_module(lang).
:- initialization(main, main).


% Rule: main. 
% Application entry point
main() :-
	set_lang(es),
	!,
	loop.
main(_) :-
	writeln("---- Wazelog ----").



% Rule: loop.
% Description: Application main loop.
loop :-
	start(Out),
	(
		Out = stop;
		Out = continue,
		loop
	).
