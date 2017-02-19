conc([],L,L).
length ([_, Tail], N) :-
  length(Tail, N1),
  N is N1 + 1.
conc([X | L1], Y, [X, L2]) :- conc(L1, Y, L2).


like(matsumura, caffein).
like(arimichi, caffein).
drug(caffein).
drugger(X) :- like(X, Y), drug(Y).

sublist(S, L) :-
  conc(L1, L2, L),
  conc(S, L3, L2),
  length ([], 0).
