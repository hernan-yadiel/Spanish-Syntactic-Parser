:- use_rendering(svgtree, [list(false)]).
:- use_module(library(tabling)).
:- table np/4, vp/4. 

%%! LEXICON %%

%%! Determiners %%
%% d0(d0(determiner), [[case], [gender, person, number], [subcategorization]])

%%! Articles
d0(d0(un), [[nom, acc, obl],[m, 3, s], [np]])   --> [un].
d0(d0(una), [[nom, acc, obl],[f, 3, s], [np]])  --> [una].
d0(d0(unos), [[nom, acc, obl],[m, 3, p], [np]]) --> [unos].
d0(d0(unas), [[nom, acc, obl],[f, 3, p], [np]]) --> [unas].
d0(d0(el), [[nom, acc, obl],[m, 3, s], [np]])   --> [el].
d0(d0(la), [[nom, acc, obl],[f, 3, s], [np]])   --> [la].
d0(d0(los), [[nom, acc, obl],[m, 3, p], [np]])  --> [los].
d0(d0(las), [[nom, acc, obl],[f, 3, p], [np]])  --> [las].

d0(d0(a, un), [[acc],[m, 3, s], [np]])   --> [a, un].
d0(d0(a, una), [[acc],[f, 3, s], [np]])  --> [a, una].
d0(d0(a, unos), [[acc],[m, 3, p], [np]]) --> [a, unos].
d0(d0(a, unas), [[acc],[f, 3, p], [np]]) --> [a, unas].
d0(d0(al), [[acc],[m, 3, s], [np]])      --> [al].
d0(d0(a, la), [[acc],[f, 3, s], [np]])   --> [a, la].
d0(d0(a, los), [[acc],[m, 3, p], [np]])  --> [a, los].
d0(d0(a, las), [[acc],[f, 3, p], [np]])  --> [a, las].

%% Null article for proper nouns.
d0(d0(), [[nom, obl],[_, 3, s], [np]])   --> [].
d0(d0(a), [[acc],[_, 3, s], [np]])       --> [a].

%%! Pronouns

d0(d0(lo), [[acc], [m,3,s], [v0]])  --> [lo].
d0(d0(la), [[acc], [f,3,s], [v0]])  --> [la].
d0(d0(los), [[acc], [m,3,p], [v0]])  --> [los].
d0(d0(las), [[acc], [f,3,p], [v0]])  --> [las].

d0(d0(pro), [[nom], [_,_,_], []])      --> [].
d0(d0(él), [[_], [m,3,s], []])      --> [él].
d0(d0(ella), [[_], [f,3,s], []])      --> [ella].


%%! Nouns
%% n0(n0(noun), [[case], [gender, person, number], [subcategorization]])
n0(n0(linguista),[_,[_,3,s], []])      --> [linguista].
n0(n0(linguistas),[_,[_,3,p], []])     --> [linguistas].
n0(n0(observatorio),[_,[m,3,s], []])   --> [observatorio].
n0(n0(biblioteca),[_,[f,3,s], []])     --> [biblioteca].
n0(n0(universidad),[_,[f,3,s], []])     --> [universidad].
n0(n0(telescopio),[_,[m,3,s], []])     --> [telescopio].
n0(n0(investigador),[_,[m,3,s], []])   --> [investigador].
n0(n0(investigadora),[_,[f,3,s], []])  --> [investigadora].
n0(n0(investigadores),[_,[m,3,p], []]) --> [investigadores].
n0(n0(investigadoras),[_,[f,3,p], []]) --> [investigadoras].
n0(n0(ingenieras),[_,[f,3,p], []])     --> [ingenieras].

%%! Proper nouns
n0(n0(angelica),[_,[f,3,s], [proper]])  --> [angelica].
n0(n0(noam),[_,[m,3,s], [proper]])  --> [noam].
n0(n0(alan),[_,[m,3,s], [proper]])  --> [alan].
n0(n0(irene),[_,[f,3,s], [proper]])  --> [irene].
n0(n0(barbara),[_,[f,3,s], [proper]])  --> [barbara].
n0(n0(boston),[_,[_,3,s], [proper]])  --> [boston].

%%! Verbs
%% v0(v0(verb), [[tense],[phi-features],[case],[complement]])
v0(v0(vivia),[[imperf],[_,3,s],[], []])        --> [vivia].
v0(v0(estaba),[[imperf],[_,3,s],[], [ap]])     --> [estaba].
v0(v0(pregunto),[[pret],[_,3,s],[], [cp]])     --> [pregunto].
v0(v0(dijo),[[pret],[_,3,s],[], [cp]])         --> [dijo].
v0(v0(vio),[[pret],[_,3,s],[acc], [dp]])       --> [vio].
v0(v0(vieron),[[pret],[_,3,p],[acc], [dp]])    --> [vieron].
v0(v0(saludo),[[pret],[_,3,s],[acc], [dp]])    --> [saludo].
v0(v0(saludaron),[[pret],[_,3,p],[acc], [dp]]) --> [saludaron].

%%! Adjectives
%% a0(a0(adjective), [[case],[phi-features]]
a0(a0(famoso), [_,[m,_, s]])    --> [famoso].
a0(a0(famosa), [_,[f,_, s]])    --> [famosa].
a0(a0(famosos), [_,[m,_, p]])   --> [famosos].
a0(a0(famosas), [_,[f,_, p]])   --> [famosas].
a0(a0(borracho), [_,[m,_, s]])  --> [borracho].
a0(a0(borracha), [_,[f,_, s]])  --> [borracha].
a0(a0(borrachos), [_,[m,_, p]]) --> [borrachos].
a0(a0(borrachas), [_,[f,_, p]]) --> [borrachas].

%%! Prepositions
%% p0(p0(preposition))
p0(p0(con), []) --> [con].
p0(p0(en), [])  --> [en].

%%! Adverbs and Intensifiers
%% x0(x0(x))
adv0(adv0(ayer)) --> [ayer].
int0(int0(muy))  --> [muy].

%%! Complentizers

c0(c0(si), [[cp]]) --> [si].
c0(c0(que), [[cp]]) --> [que].
c0(c0(), [[cp]]) --> [].

%%!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%!  RULES %%


%%! Adverbs
advbar(advbar(Adv)) --> adv0(Adv).
advp(advp(Adv)) --> advbar(Adv).

%%! Adjectives 
abar(abar(A), [_, Phi]) --> a0(A, [_, Phi]).
ap(ap(A), [_, Phi]) --> abar(A, [_,Phi]).
ap(ap(Int, A), [_, Phi]) --> int0(Int), abar(A, [_,Phi]).


%%! Nouns
% n0(n0(linguista),[[_],[_,3,s], []]) 
nbar(nbar(N), [Case,Phi, F])     --> n0(N, [Case,Phi, F]).

np(np(N), [Case,Phi, F])    --> nbar(N, [Case, Phi, F]). 
np(np(N, A), [Case,Phi, F]) --> np(N, [Case, Phi, F]), ap(A, [Case,Phi]).
np(np(A, N), [Case,Phi, F]) --> ap(A, [Case, Phi]), np(N, [Case,Phi, F]).
np(np(N, P), [Case,Phi, F]) --> np(N, [Case, Phi, F]), pp(P).


%%! Prepositions 
pbar(pbar(P, D)) --> p0(P,_), dp(D,_).
pp(pp(P)) --> pbar(P).


%%! Verbs 
% v0(v0(vio),[[pret],[_,3,s],[acc], [dp]])
vbar(vbar(V),[T,Phi])    --> v0(V,[T,Phi,[], []]).
vbar(vbar(D, V),[T,Phi]) --> d0(D, [Case,_, [v0]]), v0(V,[T,Phi, Case, [dp]]).
vbar(vbar(V, D),[T,Phi]) --> v0(V,[T,Phi, Case1, [dp]]), dp(D, [Case,_]), { subset(Case1, Case)}.
vbar(vbar(V, C),[T,Phi]) --> v0(V,[T,Phi, _, [cp]]), cp(C).
vbar(vbar(V, A), [T,Phi]) --> v0(V, [T,Phi,[], [ap]]), ap(A, [_, Phi]).

vp(vp(V), [T,Phi]) --> vbar(V, [T,Phi]).
vp(vp(V, P), [T,Phi]) --> vp(V, [T,Phi]), pp(P).
vp(vp(V, Adv), [T,Phi]) --> vp(V, [T,Phi]), advp(Adv).


%%! Determiners 
dbar(dbar(D), [Case, Phi])     --> d0(D, [Case, Phi, []]).
dbar(dbar(D, N),[Case2, Phi]) --> d0(D, [Case2, Phi, [np]]), np(N, [Case, Phi,_]), { subset(Case2, Case) }.
dp(dp(D), [Case, Phi]) --> dbar(D, [Case, Phi]).


%%! IP
i0(i0(),_) --> [].
ibar(ibar(I, V), [T, Phi]) --> i0(I, T), vp(V, [T, Phi]).
ip(ip(D, I), [Case, Phi, T]) --> dp(D, [Case, Phi]), ibar(I, [T, Phi]).
ip(ip(Adv, I), [_, _]) --> advp(Adv), ip(I,_).


%%! Complementizers

cbar(cbar(C, I)) --> c0(C, [[cp]]), ip(I, _).
cp(cp(C)) --> cbar(C).