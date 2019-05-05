:- use_rendering(svgtree, [list(false)]).
:- use_module(library(tabling)).
:- table np/4, vp/4. 


%%! phrase(cp(Tree, F), [noam, dijo, que, alan, estaba, muy, borracho, en, el, observatorio, con, el, telescopio]).
%% phrase(cp(Tree, F), [los, famosos, linguistas, saludaron, a, irene, en, la, biblioteca, de, la, universidad]).
%% phrase(cp(Tree, F), [barbara, pregunto, si, las, ingenieras, los, vieron, ayer]).
%% phrase(cp(Tree, F), [ayer, ella, dijo, que, vivia, en, boston, con, él]).
%% phrase(cp(Tree, F), [irene, vio, a, angelica]).
%% phrase(cp(Tree, F), [irene, vio, angelica]). false
%% phrase(cp(Tree, F), [irene, vio, el, telescopio]).
%% phrase(cp(Tree, F), [irene, vio, al, telescopio]).
%% phrase(cp(Tree, F), [noam, saludo, a, barbara]).
%% phrase(cp(Tree, F), [irene, dijo, que, lo, vio]).
%% phrase(cp(Tree, F), [alan, pregunto, si, angelica, llego, ayer]).
%% phrase(cp(Tree, F), [los, linguistas, estaban, borrachos]).
%% phrase(cp(Tree, F), [los, linguistas, estaban, borracho]). false
%% phrase(cp(Tree, F), [noam, estaba, en, la, biblioteca, de, la, universidad]).
%% phrase(cp(Tree, F), [noam, dijo, que, ellas, estaban, muy, borrachas]).

%%! LEXICON %%

%%! Determiners %%
%% d0(d0(determiner), [[case], [gender, person, number],[?proper] ,[subcategorization]])

%%! Articles
d0(d0(un), [[nom, acc, obl],[m, 3, s],[comm], [np]])   --> [un].
d0(d0(una), [[nom, acc, obl],[f, 3, s],[comm], [np]])  --> [una].
d0(d0(unos), [[nom, acc, obl],[m, 3, p],[comm], [np]]) --> [unos].
d0(d0(unas), [[nom, acc, obl],[f, 3, p],[comm], [np]]) --> [unas].
d0(d0(el), [[nom, acc, obl],[m, 3, s],[comm], [np]])   --> [el].
d0(d0(la), [[nom, acc, obl],[f, 3, s],[comm], [np]])   --> [la].
d0(d0(los), [[nom, acc, obl],[m, 3, p],[comm], [np]])  --> [los].
d0(d0(las), [[nom, acc, obl],[f, 3, p],[comm], [np]])  --> [las].

%%! articles in accusative
d0(d0(a, un), [[acc],[m, 3, s],[comm], [np]])   --> [a, un].
d0(d0(a, una), [[acc],[f, 3, s],[comm], [np]])  --> [a, una].
d0(d0(a, unos), [[acc],[m, 3, p],[comm], [np]]) --> [a, unos].
d0(d0(a, unas), [[acc],[f, 3, p],[comm], [np]]) --> [a, unas].
d0(d0(al), [[acc],[m, 3, s],[comm], [np]])      --> [al].
d0(d0(a, la), [[acc],[f, 3, s],[comm], [np]])   --> [a, la].
d0(d0(a, los), [[acc],[m, 3, p],[comm], [np]])  --> [a, los].
d0(d0(a, las), [[acc],[f, 3, p],[comm], [np]])  --> [a, las].

%%! articles in genitive
d0(d0(de, un), [[gen],[m, 3, s],[comm], [np]])   --> [de, un].
d0(d0(de, una), [[gen],[f, 3, s],[comm], [np]])  --> [de, una].
d0(d0(de, unos), [[gen],[m, 3, p],[comm], [np]]) --> [de, unos].
d0(d0(de, unas), [[gen],[f, 3, p],[comm], [np]]) --> [de, unas].
d0(d0(del), [[gen],[m, 3, s],[comm], [np]])      --> [del].
d0(d0(de, la), [[gen],[f, 3, s],[comm], [np]])   --> [de, la].
d0(d0(de, los), [[gen],[m, 3, p],[comm], [np]])  --> [de, los].
d0(d0(de, las), [[gen],[f, 3, p],[comm], [np]])  --> [de, las].

%% Null article for proper nouns.
d0(d0(), [[nom, obl],[_, 3, s],[proper] ,[np]])   --> [].
d0(d0(a), [[acc],[_, 3, s],[proper], [np]])       --> [a].
d0(d0(de), [[gen],[_, 3, s],[proper], [np]])       --> [de].

%%! Pronouns

%%! Clitics
d0(d0(lo), [[acc], [m,3,s],[_], [v0]])  --> [lo].
d0(d0(la), [[acc], [f,3,s],[_], [v0]])  --> [la].
d0(d0(los), [[acc], [m,3,p],[_], [v0]])  --> [los].
d0(d0(las), [[acc], [f,3,p],[_], [v0]])  --> [las].
%%! personal pronouns
d0(d0(pro), [[nom], [_,_,_],[], []])      --> [].
d0(d0(él), [[_], [m,3,s],[proper], []])      --> [él].
d0(d0(ella), [[_], [f,3,s],[proper], []])      --> [ella].
d0(d0(ellas), [[_], [f,3,p],[proper], []])      --> [ellas].


%%! Nouns
%% n0(n0(noun), [[case], [gender, person, number],[?proper] ,[subcategorization]])
n0(n0(linguista),[_,[_,3,s], [comm], [dp]])      --> [linguista].
n0(n0(linguistas),[_,[_,3,p], [comm], [dp]])     --> [linguistas].
n0(n0(observatorio),[_,[m,3,s], [comm], [dp]])   --> [observatorio].
n0(n0(biblioteca),[_,[f,3,s], [comm], [dp]])     --> [biblioteca].
n0(n0(universidad),[_,[f,3,s], [comm], [dp]])     --> [universidad].
n0(n0(telescopio),[_,[m,3,s], [comm], [dp]])     --> [telescopio].
n0(n0(investigador),[_,[m,3,s], [comm], [dp]])   --> [investigador].
n0(n0(investigadora),[_,[f,3,s], [comm], [dp]])  --> [investigadora].
n0(n0(investigadores),[_,[m,3,p], [comm], [dp]]) --> [investigadores].
n0(n0(investigadoras),[_,[f,3,p], [comm], [dp]]) --> [investigadoras].
n0(n0(ingenieras),[_,[f,3,p], [comm], [dp]])     --> [ingenieras].

%%! Proper nouns
n0(n0(angelica),[_,[f,3,s], [proper], [dp]])  --> [angelica].
n0(n0(noam),[_,[m,3,s], [proper], [dp]])  --> [noam].
n0(n0(alan),[_,[m,3,s], [proper], [dp]])  --> [alan].
n0(n0(irene),[_,[f,3,s], [proper], [dp]])  --> [irene].
n0(n0(barbara),[_,[f,3,s], [proper], [dp]])  --> [barbara].
n0(n0(boston),[_,[_,3,s], [proper], [dp]])  --> [boston].

%%! Verbs
%% v0(v0(verb), [[tense],[phi-features],[case],[complement]])
v0(v0(vivia),[[imperf],[_,3,s],[], [pp]])        --> [vivia].
v0(v0(dormia),[[imperf],[_,3,s],[], []])        --> [dormia].
v0(v0(llego),[[pret],[_,3,s],[], []])        --> [llego].
v0(v0(estaba),[[imperf],[_,3,s],[], [_]])     --> [estaba].
v0(v0(estaban),[[imperf],[_,3,p],[], [_]])     --> [estaban].
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
%% p0(p0(preposition), [case])
p0(p0(con), [obl]) --> [con].
p0(p0(en), [obl])  --> [en].

%%! Adverbs and Intensifiers
%% x0(x0(x), [Features])
adv0(adv0(ayer), [])  --> [ayer].
int0(int0(muy),  [])  --> [muy].

%%! Complentizers
%% c0(c0(C), [[subcategorization]])
c0(c0(si), [[ip]]) --> [si].
c0(c0(que), [[ip]]) --> [que].
c0(c0(), [[ip]]) --> [].

%%!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%!  RULES %%


%%! Adverbs
%% advbar(advbar(Adv), [features])
%  advp(advp(Adv), [features])
advbar(advbar(Adv), F) --> adv0(Adv, F).
advp(advp(Adv), F) --> advbar(Adv, F).

%%! Adjectives 
%%  abar(abar(A), [Case, Phi])
%   ap(ap(A), [Case, Phi])
abar(abar(A), [_, Phi]) --> a0(A, [_, Phi]).
ap(ap(A), [_, Phi]) --> abar(A, [_,Phi]).
ap(ap(Int, A), [_, Phi]) --> int0(Int,_), abar(A, [_,Phi]).


%%! Nouns
% nbar(nbar(N), [Case,Phi, F]) 
% np(np(N), [Case,Phi, F])
nbar(nbar(N), [Case,Phi, Prop])     --> n0(N, [Case,Phi, Prop, [dp]]).
nbar(nbar(N, D), [Case,Phi, Prop])     --> n0(N, [Case,Phi, Prop, [dp]]), dp(D, [[gen], _]).

np(np(N), [Case,Phi, Prop])    --> nbar(N, [Case, Phi, Prop]). 
np(np(N, A), [Case,Phi, Prop]) --> np(N, [Case, Phi, Prop]), ap(A, [Case,Phi]).
np(np(A, N), [Case,Phi, Prop]) --> ap(A, [Case, Phi]), np(N, [Case,Phi, Prop]).
np(np(N, P), [Case,Phi, Prop]) --> np(N, [Case, Phi, Prop]), pp(P,_).


%%! Prepositions 
% pbar(pbar(P, D), [Case])
% pp(pp(P), [Case])
pbar(pbar(P, D),_) --> p0(P,Case1), dp(D,[Case,_]), { subset(Case1, Case) }.
pp(pp(P),_) --> pbar(P,_).


%%! Verbs 
%% vbar(vbar(V),[Tense,Phi])
% unaccusative verbs
vbar(vbar(V),[T,Phi])    --> v0(V,[T,Phi,[], []]).
%%! clitic + verb
vbar(vbar(D, V),[T,Phi]) --> d0(D, [Case,_, [v0]]), v0(V,[T,Phi, Case, [dp]]).
%%! verb + dp complement
vbar(vbar(V, D),[T,Phi]) --> v0(V,[T,Phi, Case1, [dp]]), dp(D, [Case,_]), { subset(Case1, Case), dif(Case, [nom])}.
%%! verb + cp complement
vbar(vbar(V, C),[T,Phi]) --> v0(V,[T,Phi, _, [cp]]), cp(C,_).
%%! verb + pp complement(copulative verb)
vbar(vbar(V, P),[T,Phi]) --> v0(V,[T,Phi,_, [pp]]), pp(P,_).
%%! verb + ap complement(copulative verb)
vbar(vbar(V, A), [T,Phi]) --> v0(V, [T,Phi,[],[_]]), ap(A, [_, Phi]).

vp(vp(V), [T,Phi]) --> vbar(V, [T,Phi]).
vp(vp(V, P), [T,Phi]) --> vp(V, [T,Phi]), pp(P,_).
vp(vp(V, Adv), [T,Phi]) --> vp(V, [T,Phi]), advp(Adv,_).


%%! Determiners
%% dbar(dbar(D), [Case, Phi])
%% dp(dp(D), [Case, Phi])
% pro
dbar(dbar(D), [Case, Phi])     --> d0(D, [Case, Phi,[],[]]).
%%!  personal pronouns
dbar(dbar(D), [Case, Phi])     --> d0(D, [Case, Phi,[proper],[]]).
%%! determiner + np(articles)
dbar(dbar(D, N),[Case2, Phi]) --> d0(D, [Case2, Phi, Prop,[np]]), np(N, [Case, Phi, Prop]), { subset(Case2, Case) }.
dp(dp(D), [Case, Phi]) --> dbar(D, [Case, Phi]).


%%! IP
% i0(i0(),Features) --> [].
% ibar(ibar(I, V), [Tense, Phi])
% ip(ip(D, I), [Case, Phi, Tense])
i0(i0(imperf),[imperf]) --> [].
i0(i0(pret),[pret]) --> [].
ibar(ibar(I, V), [T, Phi]) --> i0(I, T), vp(V, [T, Phi]).
ip(ip(D, I), [Case, Phi, T]) --> dp(D, [Case, Phi]), ibar(I, [T, Phi]).
ip(ip(Adv, I), [_, _]) --> advp(Adv, _), ip(I,_).


%%! Complementizers
% cbar(cbar(C, I), Features)
% cp(cp(C), Features)
cbar(cbar(C, I),_) --> c0(C,_), ip(I, _).
cp(cp(C),_) --> cbar(C,_).