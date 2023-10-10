:- module(routes, [city/2, arco/5]).

% ---- Route Dastabse ----

% Facts: city(place, name).
% Description: Declares known cities and their names.
city(sanjose, "San Jose").
city(corralillo, "Corralillo").
city(musgoverde, "Musgo Verde").
city(tresrios, "Tres Rios").
city(cartago, "Cartago").
city(pacayas, "Pacayas").
city(paraiso, "Paraiso").
city(orosi, "Orosi").
city(cervantes, "Cervantes").
city(cachi, "Cachi").
city(juanvinias, "Juan Vinias").
city(turrialba, "Turrialba").


% Description: Declares bidirectional arcs (the graph is mixed).
two_way_arc(sanjose, corralillo, 22, 22, 44).
two_way_arc(sanjose, cartago, 20, 20, 40).
two_way_arc(corralillo, musgoverde, 6, 6, 12).
two_way_arc(musgoverde, cartago, 10, 10, 20).
two_way_arc(tresrios, pacayas, 15, 15, 30).
two_way_arc(cartago, pacayas, 13, 13, 26).
two_way_arc(pacayas, cervantes, 8, 8, 16).
two_way_arc(paraiso, orosi, 8, 8, 16).
two_way_arc(paraiso, cachi, 10, 10, 20).
two_way_arc(orosi, cachi, 12, 12, 24).
two_way_arc(cervantes, cachi, 7, 7, 14).
two_way_arc(cachi, turrialba, 40, 40, 80).


% Facts: arco(Source, Target, Cost, BestTime, WorstTime).
% Description: Declares unidirectional arcs (the graph is mixed).
arco(tresrios, sanjose, 8, 8, 16).
arco(cartago, tresrios, 8, 8, 16).
arco(paraiso, cervantes, 4, 4, 8).
arco(cervantes, juanvinias, 5, 5, 10).
arco(turrialba, pacayas, 18, 18, 36).
arco(cartago, paraiso, 10, 10, 20).
arco(juanvinias, turrialba, 4, 4, 8).


% Rule: arco(Source, Target, Cost, BestTime, WorstTime).
% Description: Decomposes bidirectional arcs into two
% unidirectional arcs, simplifying the graph logic.
arco(Source, Target, Cost, BestTime, WorstTime) :-
	two_way_arc
	(Source, Target, Cost, BestTime, WorstTime);
	two_way_arc
	(Target, Source, Cost, BestTime, WorstTime).
