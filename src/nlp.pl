:- module(nlp, [parse_user_input/2, key_nominal/2]).
:- use_module(lang).


% Rules for parsing user input
% parse_user_input(Input, Result)
% Input is a string, Result is a parse tree or a failure indication
parse_user_input(Input, Result) :-
	lex(Input, Tokens),
	expand(Tokens, Expanded),
	unbounded(Expanded, Result).



% Rule: filler(Word).
% Example:
% ?- filler(por).
% true.
% Description: Takes an atom representing a word and indicates whether the word is relevant for language analysis.
filler(Word) :-
	unclassified(Word);
	before_nominal(Word);
	contraction(Word, _).


%Regla: nominal(N).
%Ejemplo: 
%?- nominal(sanjose).
%true.
%Descripcion: Toma un atomo que representa una palabra e indica si dicha palabra es o no un sustantivo.

% Rule: nominal(Word).
% Example:
% ?- nominal(sanjose).
% true.
% Description: Takes an atom representing a word and indicates whether the word is a noun.
nominal(N) :-
	not(exclamation(N, _)), not(verbal(N)), not(filler(N)).


% Rule: expand(Tokens, Expanded).
% Example:
% ?- expand([word(al, "al"), word(del, "del"), word(alto, "Alto")], Expanded).
% Expanded = [word(a, "a"), word(el, "el"), word(de, "de"), word(el, "el"), word(alto, "Alto")].
% Description: Takes a list of words represented as `word(atom, string)` and expands contractions into their constituent words.
expand([], []).
expand([word(Contraction, _) | Tokens], NextTokens) :-
	contraction(Contraction, Expanded),
	!,
	atoms_to_words(Expanded, ExpandedWords),
	expand(Tokens, NextExpanded),
	append(ExpandedWords, NextExpanded, NextTokens).
expand([T | Tokens], [T | NextTokens]) :-
	expand(Tokens, NextTokens).


% Rule: atoms_to_words(Atoms, Words).
% Example:
% ?- atoms_to_words([sanjose, manzana, cartago],Words).
% Words = [word(sanjose, "sanjose"), word(manzana, "manzana"), word(cartago, "cartago")].
% Description: Takes a list of atoms and converts them to a list of words `word(atom, string)`.
atoms_to_words([], []) :-
	!.
atoms_to_words([Atom | Atoms], [word(Atom, Orig) | NextWords]) :-
	atom_string(Atom, Orig),
	atoms_to_words(Atoms, NextWords).



% Rule lex(Input, Tokens).
% Example:
% ?- lex("voy a San Jose", Tokens).
% Tokens = [word(voy, "voy"), word(a, "a"), word(san, "San"), word(jose, "Jose")].
% Description: Takes a string `Input` representing a sentence and obtains a list of tokens `word(atom, string)`, 
% with each token corresponding to one of the words in the sentence.
lex(Input, Tokens) :-
	string_chars(Input, Chars),
	lex(Chars, Tokens, []).
lex([], Tokens, Tokens) :-
	!.
lex([Alpha | Rest], Tokens, Previous) :-
	is_alpha(Alpha),
	!,
	lex(Rest, Tokens, Previous, [Alpha]).
lex([Space | Rest], Tokens, Previous) :-
	is_space(Space),
	!,
	lex(Rest, Tokens, Previous).
lex([Punct | Rest], Tokens, Previous) :-
	!,
	append(Previous, [punct(Punct)], Next),
	lex(Rest, Tokens, Next).
lex([Alpha | Rest], Tokens, Previous, WordChars) :-
	(
		is_alpha(Alpha);
		Alpha = '\''
	),
	!,
	append(WordChars, [Alpha], NextChars),
	lex(Rest, Tokens, Previous, NextChars).
lex(Rest, Tokens, Previous, WordChars) :-
	atom_string(WordChars, WordString),
	string_lower(WordString, Lowered),
	string_chars(Lowered, LoweredChars),
	undecorate(LoweredChars, UndecoratedChars),
	atom_chars(Undecorated, UndecoratedChars),
	append(Previous, [word(Undecorated, WordString)], Next),
	lex(Rest, Tokens, Next).



% Rule: undecorate(Cs, Us).
% Description: Takes a list of characters and obtains its version 
% without decorations (accents and dieresis) to avoid conflicts when processing data.
undecorate([], []).
undecorate(['a' | Cs], ['a' | Us]) :- !, undecorate(Cs, Us).
undecorate(['e' | Cs], ['e' | Us]) :- !, undecorate(Cs, Us).
undecorate(['i' | Cs], ['i' | Us]) :- !, undecorate(Cs, Us).
undecorate(['o' | Cs], ['o' | Us]) :- !, undecorate(Cs, Us).
undecorate(['u' | Cs], ['u' | Us]) :- !, undecorate(Cs, Us).
undecorate([C | Cs], [C | Us])     :- undecorate(Cs, Us).


% Rule: classify(word(Atom, Orig), Type).
% Example:
% ?- classify(word(encuentro, "encuentro"), Type).
% Type = verbal(encuentro).
% Description: Classifies words according to their function in a sentence, whether verbal, nominal, exclamation or filler.
classify(punct(_), punct).
classify(word(Atom, Orig), filler(Atom, Orig)) :-
	filler(Atom),
	!.
classify(word(Atom, _), verbal(Atom)) :-
	verbal(Atom),
	!.
classify(word(Atom, _), exclamation(Type)) :-
	exclamation(Atom, Type),
	!.
classify(word(Atom, Original), nominal(Atom, Original)).


rarquica.

% Rule: clause(sentence).
% Example:
% Description: Succeeds only if the sentence in question is a valid sentence. 
% This occurs for all valid subexpressions, except for isolated verbal forms without hierarchical association.
clause(verbal(_)) :-
	!,
	fail.
clause(Clause) :-
	well_formed(Clause).



% Rule: well_formed(Expression).
% Example:
% ?- well_formed(nominal(yo,"yo","yo")).
% true.
% Description: Evaluates whether an expression is well formed. The expression can be verbal, nominal, exclamation, or other type.
well_formed(exclamation(_)).
well_formed(verbal([_ | _])).
well_formed(nominal('', _, _)) :-
	!,
	fail.
well_formed(nominal(_, _, _)).
well_formed(svo(S, V, O)) :-
	(not(well_formed(V)); well_formed(O)),
	(well_formed(V); well_formed(S)).




% Rule: ast_join(Ast, Term, NextAst).
% Description: Builds a syntax tree, with some semantic interpretations included, 
% from a previous state of the same tree and a next component to add. The atom `nomatch` 
% is used as a previous tree to indicate that there was not one previously.
% ---- AST ----
ast_join(nomatch, nominal(A, Orig), nominal(A, Orig, Orig)).
ast_join(nomatch, verbal(V), verbal([V])).
ast_join(nomatch, filler(F, Orig), nominal('', Orig, "")) :-
	before_nominal(F).
ast_join(nominal(LA, LOrig, LBare), nominal(RA, ROrig), nominal(NextA, NextOrig, NextBare)) :-
	nominal_join(nominal(LA, LOrig, LBare), nominal(RA, ROrig), nominal(NextA, NextOrig, NextBare)).
ast_join(nominal(A, Orig, Bare), verbal(V), svo(nominal(A, Orig, Bare), verbal([V]), nominal('', "", ""))).
ast_join(nominal(A, LeftOrig, Bare), filler(F, RightOrig), nominal(A, NextOrig, Bare)) :-
	before_nominal(F),
	nominal_join(nominal(A, LeftOrig, Bare), nominal('', RightOrig), nominal(A, NextOrig, _)).
ast_join(verbal(V), nominal(A, Orig), svo(nominal('', "", ""), verbal(V), nominal(A, Orig, Orig))).
ast_join(verbal(LeftV), verbal(RightV), verbal(NextV)) :-
	append(LeftV, [RightV], NextV).
ast_join(verbal(V), filler(F, Orig), svo(nominal('', "", ""), verbal(V), nominal('', Orig, ""))) :-
	before_nominal(F).
ast_join(svo(S, verbal(LeftV), nominal('', "", "")), verbal(RightV), svo(S, verbal(NextV), nominal('', "", ""))) :-
	!,
	ast_join(verbal(LeftV), verbal(RightV), verbal(NextV)).
ast_join(svo(S, V, O), verbal(Verbal), svo(S, V, svo(O, verbal([Verbal]), nominal('', "", "")))) :-
	!.
ast_join(svo(S, V, O), Term, svo(S, V, NextO)) :-
	!,
	ast_join(O, Term, NextO).
ast_join(Tree, filler(_, _), Tree).



% Rule: nominal_join(nominal(LeftA, LeftOrig, LeftBare), nominal(RightA, RightOrig), nominal(NextA, NextOrig, NextBare)).
% Description: Concatenates two nominal forms into a compound nominal.
nominal_join(nominal(LA, LOrig, LBare), nominal(RA, ROrig), nominal(NextA, NextOrig, NextBare)) :-
	atom_concat(LA, RA, NextA),
	append_space(LOrig, OrigWithSpace),
	append_space(LBare, BareWithSpace),
	string_concat(OrigWithSpace, ROrig, NextOrig),
	string_concat(BareWithSpace, ROrig, NextBare).



% Rule append_space(WithoutSpace, WithSpace).
% Description: Adds a space at the end of a string only if the input is not the empty string.
append_space("", "") :-
	!.
append_space(String, WithSpace) :-
	string_concat(String, " ", WithSpace).



% Rule: unbounded(Tokens, Result).
% Description: Parses a complete (unbounded) input, that is, a sentence that is not delimited by punctuation.
unbounded(Tokens, Result) :-
	unbounded(Tokens, [], Result).
unbounded([], Sentences, ok(Sentences)) :-
	!.
unbounded([punct(Sep) | Tokens], Previous, Result) :-
	sentence_sep(Sep),
	!,
	unbounded(Tokens, Previous, Result).
unbounded(Tokens, Previous, Result) :-
	sentence(Tokens, Rest, Sentence),
	!,
	append(Previous, [Sentence], Next),
	unbounded(Rest, Next, Result).
unbounded([FailureHead | _], _, fail(FailureHead)).



% ---- Sentence ----
% Rule: sentence(tokens, Resto, Oracion).
% Example:
% ?- sentence([word(yo, "Yo"), word(estoy, "estoy"), word(en, "en"), word(cartago, "Cartago"), punct('.')], R, S).
% Description: Parses a sentence based on its input. The output is both the sentence 
% and the list of tokens that follow it and that must then be parsed as more sentences.
sentence(Tokens, Rest, Sentence) :- sentence(Tokens, Rest, Sentence, nomatch).
sentence([], [], Sentence, Sentence) :-
	!,
	clause(Sentence).
sentence([punct(Sep) | Rest], Rest, Sentence, Acc) :-
	sentence_sep(Sep),
	!,
	sentence([], [], Sentence, Acc).
sentence([word(Exclamation, _) | Tokens], Rest, Sentence, nomatch) :-
	exclamation(Exclamation, Type),
	!,
	sentence(Tokens, Rest, Sentence, exclamation(Type)).
sentence([word(Word, _) | Tokens], Rest, Sentence, exclamation(E)) :-
	not(verbal(Word)),
	!,
	sentence(Tokens, Rest, Sentence, exclamation(E)).
sentence(Rest, Rest, exclamation(E), exclamation(E)) :-
	!.
sentence([T | Tokens], Rest, Sentence, Ast) :-
	classify(T, Term),
	ast_join(Ast, Term, NextAst),
	sentence(Tokens, Rest, Sentence, NextAst).



% Rule: key_nominal(SVO, nominal(A, Orig, Bare)).\
% Description: The rule takes a sentence represented in SVO as a subject-verb-object 
% structure and looks for its complement noun. Mostly used to obtain the name of a city,
% which is always found in the complement position in the active voice. 
key_nominal([nominal(A, Orig, Bare)], nominal(A, Orig, Bare)) :-
	!.
key_nominal([svo(_, _, O)], N) :-
	!,
	key_nominal([O], N).
key_nominal([_ | Es], N) :-
	key_nominal(Es, N).
