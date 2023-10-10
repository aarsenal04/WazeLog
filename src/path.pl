:- module(path, [shortest_path_through/4]).
:- use_module(library(heaps)).
:- use_module(library(dicts)).
:- use_module(routes).



% Rule: shortest_path_through(Source, Stops, Target, Result).
% Note: This rule is the public interface to the Dijkstra algorithm.\
% ---- Use intermiddiate stops to find the shortest path between two nodes. ----
shortest_path_through(Source, [], Target, shortest_path(Path, Cost)) :-
	shortest_path(Source, Target, Path, Cost),
	!.
shortest_path_through(Source, [], Target, no_route(Source, Target)) :-
	!.
shortest_path_through(Source, [Stop | Stops], Target, Result) :-
	shortest_path(Source, Stop, FirstPath, cost(FirstCost, FirstBestTime, FirstWorstTime)),
	!,
	shortest_path_through(Stop, Stops, Target, NextResult),
	(
		NextResult = shortest_path([Stop | NextPath], cost(NextCost, NextBestTime, NextWorstTime)),
		Cost is FirstCost + NextCost,
		BestTime is FirstBestTime + NextBestTime,
		WorstTime is FirstWorstTime + NextWorstTime,
		append(FirstPath, NextPath, Path),
		Result = shortest_path(Path, cost(Cost, BestTime, WorstTime));

		NextResult = Result
	),
	!.
shortest_path_through(Source, [Stop | _], _, no_route(Source, Stop)).


% Rule: shortest_path(Source, Target, Path, Cost).
% Description: Finds the shortest path between two nodes.
shortest_path(Source, Target, Path, Cost) :-
	empty_heap(Empty),
	add_to_heap(Empty, 0, Source, Heap),
	dict_create(Initial, nodes, [Source: node('', 0, unvisited)]),
	shortest_path(Source, Target, Heap, Initial, Nodes),
	traceback(Source, Target, Nodes, ReversePath, Cost),
	reverse(ReversePath, Path),
	!.



% Rule: traceback(Source, Target, Nodes, Path, Cost).
% Description: Reconstructs a path and its cost from a solution
% given by the Dijkstra algorithm.
traceback(Source, Source, _, [Source], cost(0, 0, 0)) :-
	!.
traceback(Source, Target, Nodes, [Target | ReversePath], cost(Cost, BestTime, WorstTime)) :-
	get_dict(Target, Nodes, node(Parent, Cost, _)),
	arco(Parent, Target, _, EdgeBestTime, EdgeWorstTime),
	traceback(Source, Parent, Nodes, ReversePath, cost(_, NextBestTime, NextWorstTime)),
	BestTime is EdgeBestTime + NextBestTime,
	WorstTime is EdgeWorstTime + NextWorstTime.

shortest_path(Source, Target, Heap, Known, Nodes) :-
	get_from_heap(Heap, Distance, Key, NextHeap),
	!,
	get_dict(Key, Known, State),
	visit(Source, Target, Key, Distance, State, NextHeap, Known, Nodes).


% Rule: visit(Source, Target, actual, distancia, nodo, pila, nodos, SiguientesNodos).
% Description: Visits a node (part of the Dijkstra algorithm).
visit(_, Target, Target, Distance, node(_, Distance, unvisited), _, Nodes, Nodes) :-
	!.
visit(Source, Target, Key, Distance, node(Parent, Distance, unvisited), Heap, Known, Nodes) :-
	!,
	put_dict(Key, Known, node(Parent, Distance, visited), KnownWithVisited),
	findall(Neighbor, arco(Key, Neighbor, _, _, _), Neighbors),
	test_neighbors(Neighbors, Key, Distance, Heap, KnownWithVisited, NextHeap, NextKnown),
	visit(Source, Target, Key, Distance, node(Parent, Distance, visited), NextHeap, NextKnown, Nodes).
visit(Source, Target, _, _, _, Heap, Known, Nodes) :-
	shortest_path(Source, Target, Heap, Known, Nodes).



% Add all neighbors to the heap and update their best known parent.
test_neighbors([], _, _, Heap, Known, Heap, Known).
test_neighbors([Neighbor | Neighbors], Key, Distance, Heap, Known, NextHeap, NextKnown) :-
	get_dict(Neighbor, Known, State),
	!,
	override_distance(Key, Distance, Neighbor, State, Heap, Known, HeapWithNeighbor, KnownWithNeighbor),
	test_neighbors(Neighbors, Key, Distance, HeapWithNeighbor, KnownWithNeighbor, NextHeap, NextKnown).
test_neighbors([Neighbor | Neighbors], Key, Distance, Heap, Known, NextHeap, NextKnown) :-
	FakeNode = node('', inf, unvisited),
	override_distance(Key, Distance, Neighbor, FakeNode, Heap, Known, HeapWithNeighbor, KnownWithNeighbor),
	test_neighbors(Neighbors, Key, Distance, HeapWithNeighbor, KnownWithNeighbor, NextHeap, NextKnown).


% Rule: override_distance(actual, distancia, vecino, nodo, pila, nodos, NuevaPila, NuevosNodos).
% Description: If the distance to a node through another node
% turns out to be less, then it changes its best known parent.
% Otherwise it does nothing. Part of the Dijkstra algorithm.
override_distance(Key, Distance, Neighbor, node(_, OldDistance, unvisited), Heap, Known, NextHeap, NextKnown) :-
	arco(Key, Neighbor, Cost, _, _),
	NewDistance is Distance + Cost,
	NewDistance < OldDistance,
	!,
	add_to_heap(Heap, NewDistance, Neighbor, NextHeap),
	put_dict(Neighbor, Known, node(Key, NewDistance, unvisited), NextKnown).
override_distance(_, _, _, _, Heap, Known, Heap, Known).
