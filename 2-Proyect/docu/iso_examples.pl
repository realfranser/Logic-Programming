% :- module(_,_).
:- module(_,_,[classic,assertions,rtchecks]).
% :- use_package(['bf/bfall']). % Nice for grammars, rest better off

:- use_package([fsyntax]). 
% :- use_package([functional]). 

:- set_prolog_flag(multi_arity_warnings,off).
% :- set_prolog_flag(single_var_warnings,off).
:- use_module(library(format)).
:- use_module(library(lists),[append/3]).
:- use_module(library(aggregates)).
% -------------------------------------------------------------------
% Occurs-check vs. cyclic terms (not bfall)

cycle(X) :-
    X = f(X),
    Y = f(Y),
    X=Y.

% (not really necessary:) set_prolog_flag(check_cycles,on).
% write(X).

% -------------------------------------------------------------------
% Operators

:- op(500,xfx,is_father_of).

%% is_father_of(john, peter).
%% is_father_of(peter,jim).

john  is_father_of  peter. 
peter is_father_of  jim.


%% :- op(1100,fx,[load,mult,bne]).
%% :- op( 500,fx,@).
%% :- op( 500,xfx,@).
%% 
%% label1 : load @r3,r1.
%%          load 5,r2.
%%          mult r1,r2.
%%          bne  0,label1.

% -------------------------------------------------------------------
% The execution rules / search tree / debugger

grandparent(C,G) :-
    parent(C,P),
    parent(P,G).

parent(C,P) :- 
    father(C,P).
parent(C,P) :- 
    mother(C,P).

father(charles, philip).
father(ana, george).

mother(charles, ana).

% -------------------------------------------------------------------

permutation([], []).
permutation([H|T], [H1|T1]):- 
    select_el(H1, [H|T], R),
    permutation(R, T1). 

select_el(H, [H|T], T).
select_el(H1, [H|T], [H|T1]):-
    select_el(H1, T, T1).

% Queries:
% permutation([a,b,c], L). 
% select_el(X, [a,b,c]). 

% -------------------------------------------------------------------

mylist([_|T]) :- mylist(T).
mylist([]).

mymember(X,[X|_]).
mymember(X,[_|T]) :- 
    mymember(X,T).

% Queries.
% mymember(c,[a,b,c]).
% mymember(d,[a,b,c]).
% mymember(X,[a,b,c]).

mymember2(X,Y) :-
    Y = [X|_].
mymember2(X,Y) :- 
    Y = [_|T],
    mymember2(X,T).

% -------------------------------------------------------------------

%% Module system: see files

% -------------------------------------------------------------------
% Arithmetic

% X is 3+2**2.

% X = 7.0 ? 
%% Examples of failure:
% X=3, Y=4, Y < X+1.
% X=3, Y=4, X is Y+1.
% X=3, Y=4, X =:= Y.

%% Examples of error: 
% Y=4, Y < a+1.
% X is Z+1.
% X=3, X =:= f(a).



% -------------------------------------------------------------------
% pfactorial(s(s(s(s(s(0))))),_).
% pfactorial(s(s(s(s(s(s(0)))))),_).
% factorial(6,_).
% factorial(1000,_).

pfactorial(0,s(0)).
pfactorial(s(N),F):-
    pfactorial(N,F1),
    ptimes(s(N),F1,F).

ptimes(0,_,0).
ptimes(s(X),Y,Z) :- pplus(W,Y,Z), ptimes(X,Y,W).

pplus(0,Y,Y) :- nat(Y).
pplus(s(X),Y,s(Z)) :- pplus(X,Y,Z).

 %% Multiple uses: 
 %% pplus(s(s(0)),s(0),Z).
 %% pplus(s(s(0)),Y,s(0)).
 %% pplus(s(0),Y,s(s(s(0)))).
 %%
 %% Multiple solutions: 
 %% pplus(X,Y,s(s(s(0)))).

factorial(0,1). 
factorial(N,F):-
    N > 0,
    N1 is N-1,
    factorial(N1,F1),
    F is F1*N.

% Queries:
% factorial(3,F).
% factorial(N,6). % ERROR!

wrong_factorial(0,1). 
wrong_factorial(N,F):-
    N > 0,
    N1 is N-1,
    F is F1*N,
    wrong_factorial(N1,F1).

% wrong_factorial(3,F).

% -------------------------------------------------------------------

plus(X,Y,Z):- Z is X + Y.

% Queries
% plus(4,5,Z).
% plus(X,5,9).

% -------------------------------------------------------------------

plus2(X,Y,Z) :- number(X),number(Y), Z is X + Y.
plus2(X,Y,Z) :- number(X),number(Z), Y is Z - X.
plus2(X,Y,Z) :- number(Y),number(Z), X is Z - Y.
plus2(_,_,_) :- write('Error: not sufficiently instantiated.\n').

% Queries
% plus2(4,5,Z).
% plus2(X,5,9).
% plus2(X,Y,9).

%% plu(X,Y,X) :- number(X), plus2....
%% plu(X,Y,X) :- nat(X), padd... 

% -------------------------------------------------------------------

% _T=date(9,February,1947), arg(3,_T,X).
% _T=date(9,February,1947), _T=date(_,_,X).
% functor(Array,array,5), arg(1,Array,black), arg(5,Array,white).      
% arg(2, [a,b,c,d], X).
% -------------------------------------------------------------------

% -------------------------------------------------------------------

add_arrays(A,B,C):-
    functor(A,array,N),
    functor(B,array,N),
    functor(C,array,N),
    add_elements(N,A,B,C).

add_elements(0,_A,_B,_C).
add_elements(I,A,B,C):-
    I>0, 
    arg(I,A,AI), 
    arg(I,B,BI), 
    arg(I,C,CI),
    CI is AI + BI, 
    I1 is I - 1,
    add_elements(I1,A,B,C).

%% add_arrays(array(1,2,3),array(4,5,6),R).
%% add_arrays(array(1,2,3),array(4,5,6),array(5,7,9)).
%% add_arrays(array(1,2,3),array(4,5),R).

add_lists([],[],[]).
add_lists([X|Xs],[Y|Ys],[Z|Zs]):-
    Z is X + Y,
    add_lists(Xs,Ys,Zs).

%% add_lists([1,2,3],[4,5,6],R).
%% add_arrays_lists([1,2,3],[4,5,6],[5,7,9]).
%% add_lists([1,2,3],[4,5],R).

% ------------------------------

:- op(250,xfx,@).
%% (*** Needs functional package above)
:- fun_eval '@'/2.

@(T,N) := A :- arg(N,T,A).
array(A,N) :- functor(A,array,N).

add_arr(A,B,C):-    
    array(A,N), 
    array(B,N), 
    array(C,N),
    add_els(N,A,B,C).

add_els(0,_,_,_).
add_els(I,A,B,C) :-
    I>0, 
    C@I is A@I + B@I, 
    add_els(I-1,A,B,C).

%% :- set_prolog_flag(read_postfix_blocks, on).
%% :- op(50,yf,([])).
%% :- fun_eval '@'/2.
%% 
%% '\006\postfix_block'(f,[4]) 
%% array(A,N) :- functor(A,array,N).
%% 
%% X = f[4]
%% '\006\postfix_block'(f,[4]) 
    

% ------------------------------

subterm(Term,Term).
subterm(Sub,Term):- 
    functor(Term,_F,N), 
    subterm_args(N,Sub,Term).

subterm_args(N,Sub,Term):-     
    arg(N,Term,Arg),   % also checks N > 0 (arg/1 fails otherwise!)
    subterm(Sub,Arg).
subterm_args(N,Sub,Term):- 
    N>1, 
    N1 is N-1, 
    subterm_args(N1,Sub,Term).

%% subterm( f(a) , g(b,f(a)) ).
%% subterm( f(b) , g(b,f(a)) ).
%% subterm( g(b,f(a)) , g(b,f(a)) ).
%% subterm( X , g(b,f(a)) ).
%% subterm( f(X) , g(b,f(a)) ).
%% subterm( X , g(X,f(a)) ).
%% subterm( f(X) , g(b,f(X)) ).

% -------------------------------------------------------------------

%% date(9,february,1947) =.. L.
%% T =.. [date,9,february,1947].
%% _F = '+', X =.. [_F,a,b].

deriv(sin(X), X, cos(X)). 
deriv(cos(X), X, -sin(X)). 

deriv(FG_X,   X, DF_G * DG_X):-
    FG_X =.. [_, G_X],
    deriv(FG_X, G_X, DF_G),
    deriv(G_X, X, DG_X).

% deriv(sin(cos(x)),x,D).

%-------------------------------------------------------------------
/*

?- name('1', X), name(Y, X).

X = [49],
Y = 1 ? 

yes 

?- atom_codes(7, L).
{ERROR: atomic_basic:atom_codes/2, arg 1 - expected an non-numeric atom, found 7}

no
?- atom_codes('7', L).

L = [55] ? 

yes
?- number_codes('7', L).
{ERROR: atomic_basic:number_codes/2, arg 1 - expected a number, found 7}

no
?- number_codes(7, L).

L = [55] ? 

yes
?- number_codes(X, [55]).

X = 7 ? 

yes
?- atom_codes(X, [55]).

X = '7' ? 

yes

?- set_prolog_flag(write_strings,on).
*/

%---------------------------------------------------------------------


sumlist([],0).
sumlist([X|Xs],S) :-
    sumlist(Xs,SXs),
    S is X+SXs.

% -----

gsumlist(L,S) :- 
    gsumlist_(L, 0, S).

gsumlist_([], S, S).
gsumlist_([X|Xs], A, S) :-
    NS is A+X,
    gsumlist_(Xs, NS, S).
    

%---------------------------------------------------------------------

len([],0).
len([_|T],N) :-
    len(T,TN),
    N is TN+1.

% -----------

glen(L,Len) :-
    glen_(L,0,Len).

glen_([],Len,Len).
glen_([_|Xs],S,Len) :-
    S1 is 1+S,
    glen_(Xs,S1,Len).

%---------------------------------------------------------------------

list_length(Xs,N):- 
    var(Xs),
    integer(N),
    create_list(N,Xs).
    % var(Xs), _ is N, create_list(N,Xs).
list_length(Xs,N):- 
    nonvar(Xs),
    compute_length(Xs,N).

create_list(0,[]).
create_list(N,[_|Xs]):- 
    N > 0,
    N1 is N - 1,
    create_list(N1,Xs).

%% compute_length([],0).
%% compute_length([_|T],N):-
%%     compute_length(T,TN),
%%     N is TN+1.

compute_length(L,N) :-
    compute_length_(L,0,N).

compute_length_([],N,N).
compute_length_([_|T],A,N) :-
    NA is A+1,
    compute_length_(T,NA,N).

% length_list(L,3).
% -------------------------------------------------------------------
subterm_ng(Sub,Term) :-
    Sub == Term.
subterm_ng(Sub,Term):- 
    nonvar(Term),
    functor(Term,_F,N), 
    subterm_ng_(N,Sub,Term).

subterm_ng_(N,Sub,Term):-     
    arg(N,Term,Arg),   % also checks N > 0 (arg/1 fails otherwise!)
    subterm_ng(Sub,Arg).
subterm_ng_(N,Sub,Term):- 
    N>1, 
    N1 is N-1, 
    subterm_ng_(N1,Sub,Term).

%% subterm_ng( f(a) , g(b,f(a)) ).
%% subterm_ng( f(b) , g(b,f(a)) ).
%% subterm_ng( g(b,f(a)) , g(b,f(a)) ).
%% subterm_ng( X , g(b,f(a)) ).
%% subterm_ng( f(X) , g(b,f(a)) ).
%% subterm_ng( X , g(X,f(a)) ).
%% subterm_ng( f(X) , g(b,f(X)) ).
% -------------------------------------------------------------------

% insert_unif([], a, L).
% insert_unif([a,b,e], c, L).
% insert_unif([a,b,e], X, L).

insert_unif([], Item, [Item]).
insert_unif([H|T], Item, [H|T]) :- H = Item.
insert_unif([H|T], Item, [Item, H|T]) :- H @> Item.
insert_unif([H|T], Item, [H|NewT]) :- H @< Item, insert_unif(T, Item, NewT).

% insert_ee([], a, L).
% insert_ee([a,b,e], c, L).
% insert_ee([a,b,e], X, L).

insert_ee([], Item, [Item]).
insert_ee([H|T], Item, [H|T]) :- H == Item.
insert_ee([H|T], Item, [Item, H|T]) :- H @> Item.
insert_ee([H|T], Item, [H|NewT]) :- H @< Item, insert_ee(T, Item, NewT).

% --------------------------------------
% I/O

write_list_to_file(L,F) :- 
    telling(OldOutput),             % Grab current output stream.
    tell(F), write_list(L), told,   % Write into F, close.
    tell(OldOutput).                % Reset previous output stream.

write_list([]).
write_list([X|Xs]):- 
    write_canonical(X), 
    write('.'), 
    nl, 
    write_list(Xs).
%% write_list([X|Xs]):- write(X), nl, write_list(Xs).


% --------------------------------------
% Examples of cut.
% Queries: s(A),p(B,C).
%          s(A),B=c,p(B,C).

s(1).                
s(2).

l(1).    

m(c).

r(8).  
r(9).

p(X,Y):-
    l(X),
    l(Y).
p(X,Y):-
    r(X),
    !,
    r(Y).
p(X,_):-
    m(X).


% --------------------------------------
% white cuts.

white_max(X,Y,X):-
    X > Y,
    !.
white_max(X,Y,Y):-
    X =< Y.

% --------------------------------------
address(X,Add):-
    home_address(X,Add), 
    !.
address(X,Add):-
    business_address(X,Add).

home_address(a,'foo').

business_address(a,'bar').
business_address(b,'baz').

% --------------------------------------
% green cuts.
member_normal(X,[X|_]).
member_normal(X,[_|T]) :-
    member_normal(X,T).

membercheck(X,[X|_]) :- !.
membercheck(X,[_|T]) :-
    membercheck(X,T).

% --------------------------------------

% red cuts.
red_max(X,Y,X):-
    X > Y,
    !. % Red cut!
red_max(_X,Y,Y). 
    % X =< Y. % Missing!

% ?- red_max(5,2,M).
% ?- red_max(2,5,M).
% ?- red_max(5,2,2).

new_red_max(X,Y,Z) :- 
    X > Y, 
    !, 
    Z = X.
new_red_max(_,Y,Z) :- 
    % X =< Y, 
    Z = Y.

% Syntactic sugar: 
if_then_else_max(X,Y,M) :- ( X>Y -> M=X ; M=Y).

%% case x
%%     x=a : y=1 ...
%%     x=b : y=2 ...
%%     x=c : y=3 ...

% "Case"
case(a,Y) :- !, Y=1.
case(b,Y) :- !, Y=2.
case(c,Y) :- !, Y=3.

%% case(a,1).
%% case(b,2).
%% case(c,3).


% ?- days_in_year(X,366).
% ?- days_in_year(4,D).
% ?- days_in_year(3,366).
% ?- days_in_year(4,366).
% ?- days_in_year(4,365).
% ?- days_in_year_good(4,365).
% ?- days_in_year_good(4,366).
% ?- days_in_year_good(4,D).
% ?- days_in_year_assrt(a,D).
% ?- days_in_year_moded(4,365).
% ?- days_in_year_assrt(4,366).
% ?- days_in_year_assrt(4,365).
% ?- days_in_year_assrt(a,D).

days_in_year(X,366) :-
    leap_year(X),
    !.
days_in_year(_X,365).

%% days_in_year(X,D) :-
%%     leap_year(X),
%%     !, 
%%     D=366.
%% days_in_year(_X,365).

leap_year(X):-
    number(X),
    0 is X mod 4.
%% standard_year(X):-
%%     number(X),
%%     R is X mod 4,
%%     R \= 0.

days_in_year_a(X,D) :-
    leap_year(X),
    !,
    D = 366.
days_in_year_a(_X,365).

days_in_year_moded(_,D) :-
    nonvar(D),
    !,
    write('ERROR: illegal var in second argument.\n'),
    abort.
days_in_year_moded(X,366) :-
    leap_year(X),
    !.
days_in_year_moded(_X,365).


:- pred days_in_year_assrt(X,D) : (int(X),var(D)).

days_in_year_assrt(X,366) :-
    leap_year(X),
    !.
days_in_year_assrt(_X,365).


% --------------------------------------
% more examples of cut. Splitting a list.
split([],[],[]).
split([X|L],[X|L1],L2):-
    X >= 0, !,
    split(L,L1,L2).
split([X|L],L1,[X|L2]):-
    % X < 0,
    split(L,L1,L2).

% First, test it without cut
% Then,  test again with cut
% queries: split([1,-1,3],L1,L2).
%          split([1,-1,3],L1,L2),!.
% --------------------------------------

k_1sol(j,k). 
k_1sol(X,Y) :- k(X), !, z(Y).
k_1sol(l,m). 

k(a).
k(b).

z(1).
z(2).


% -------------------------------------------------------------------
% Meta-calls: call/1 example
% P=q(X), p1(P).
% P=member(Y,[1,2,3]), p1(P).

p1(X) :- call(X). 

q(a).
q(b).

% -------------------------------------------------------------------
% Meta-calls: call/1 example - apply
% apply(p,[a]).
% apply(p,[X]).

apply(P,Args) :-
    G =.. [P|Args],
    call(G).

p(a).
p(b).

:- use_package(hiord).

% P=append, P([1],[2],Y).

maplist(_P,[]).
maplist(P,[Tuple|RTuples]) :-
    apply(P,Tuple),
    maplist(P,RTuples).

% query: 
% ?- maplist(p,[[a],[b]]).
% ?- maplist(append, [ [ [a],[c,d], X ],
%                      [ X,  [e,f], Y ],
%                      [ X,  Y,     Z ]  ]).

maplistho(_P,[]).
maplistho(P,[[X,Y,Z]|RTuples]) :-
    P(X,Y,Z),
    maplistho(P,RTuples).

% ?- maplistho(append,[ [ [a],[c,d], X ],
%                      [ X,  [e,f], Y ],
%                      [ X,  Y,     Z ]  ]).


% -------------------------------------------------------------------
% Meta-calls: setof/3, bagof/3, findall/3 examples

:- dynamic likes/2.

likes(bill,  cider).
likes(dick,  beer).
likes(tom,   beer).
likes(tom,   beer).
likes(tom,   cider).
likes(harry, beer).
likes(jan,   cider).

%% setof(X, likes(X,Y), S).
%% setof(X, Y^likes(X,Y), S).
%% bagof(X, likes(X,Y), S).
%% findall(X, likes(X,Y), S).
%% bagof(X, Y^likes(X,Y), S).
%% bagof(X, likes(X,water),S).      % fails
%% findall(X, likes(X,water),S).
%% findall(beer_lover(X), likes(X,beer), S).

%% setof(X, member(X,[d,a,b,c,b,d]), L).
%% bagof(X, member(X,[d,a,b,c,b,d]), L).
%% findall(X, (member(X,[d,a,b,c,b,d]), X @< c), L).
%% findall(  Y-L   ,  setof(X,likes(X,Y),L),  Pairs).

nat(0).
nat(s(X)) :- nat(X).

% -------------------------------------------------------------------
% Meta-calls: setof/3 example.
% subset([1,2,3],S).
% powerset([1,2,3],Pset).

subset([],[]).
subset([X|Xs],[X|Ys]):-
    subset(Xs,Ys).
subset([_|Xs],Ys):-
    subset(Xs,Ys).

powerset(Set,Pset):-
    setof(X,subset(Set,X),Pset).

% -------------------------------------------------------------------
% Negation as failure
% Joe is an unmarried student!
% John is NOT an unmarried student!

unmarried_student(X) :- 
    \+ married(X), 
    student(X).

student(joe).
married(john).

better_unmarried_student(X) :- 
    not(married(X)), 
    student(X).

not(G) :-
    ground(G), !,
    \+ G. 
not(G) :-
    write('ERROR: Non-ground goal in negation: '), write(G), nl,
    abort.

not_(G) :- 
    call(G), 
    !,  % Red cut!
    a=b. % fail
not_(_G).

:- pred not__(G) : ground(G).

not__(G) :- \+ G.


overlap(S1,S2):-  % S1 and S2 overlap if they share an element
    member(X,S1), member(X,S2).

disjoint(S1,S2):- \+ overlap(S1,S2).

once(X) :- call(X), !.

% -------------------------------------------------------------------
% cut-fail

myfail :- a=b.

fground(Term):- 
    var(Term), 
    !, 
    fail.
fground(Term):- 
    nonvar(Term), 
    functor(Term,_F,N),
    fground(N,Term).

fground(0,_T).       %% All subterms traversed
fground(N,T):- 
    N>0, 
    arg(N,T,Arg), 
    fground(Arg), 
    N1 is N-1, 
    fground(N1,T).

% Comment out the first clause of fground/1 and guess what happens.

%% fground_(0,T).       %% All subterms traversed
%% fground_(N,T):- 
%%     N>0, 
%%     fground(~arg(N,T)), 
%%     fground_(N-1,T).

%% debug: fground(f(a,f(c,d),Z)).

% ----------------------------------------------------

:- dynamic related/2.

related(1,2).

relate_numbers(X, Y):- assert(related(X, Y)).
unrelate_numbers(X, Y):- retract(related(X, Y)).

/* 
related(1, 2).
relate_numbers(1, 2).
related(1, 2).
unrelate_numbers(1, 2).
related(1, 2).
*/


% -------------------------------------------------------------------
% Dynamic program modification: Using lemmas.
% 0 1 1 2 3 5 8 ...

fib(0, 0). 
fib(1, 1). 
fib(N, F):-  
    N > 1,        
    N1 is N - 1, 
    N2 is N - 2, 
    fib(N1, F1), 
    fib(N2, F2),  
    F is F1 + F2. 

% -------------------------------------------------------------------

lfib(N, F):-
    lemma_fib(N, F),
    !. 
lfib(N, F):- 
    N > 1,   
    N1 is N - 1,  
    N2 is N - 2, 
    lfib(N1, F1), 
    lfib(N2, F2), 
    F is F1 + F2,     
    assert(lemma_fib(N, F)). 

:- dynamic lemma_fib/2.
lemma_fib(0, 0). 
lemma_fib(1, 1). 

%% ?- fib(30,Y).
%% ?- fib(31,Y).
%% ?- lfib(30,Y).
%% ?- lfib(31,Y).
%% ?- lfib(200,Y).
%% ?- lfib(1000,Y).


% -------------------------------------------------------------------
% Meta-interpreter
% ----------------

% % Ciao-specific: makes dynamic clauses visible to clause/2
:- use_package(dynamic_clauses).
:- redefining(assert/1).
:- redefining(retract/1).

c(H,B) :- clause(H,B).

% Visible to clause/2 
:- dynamic lappend/3.

lappend([],X,X).
lappend([X|Y],Z,[X|W]) :- lappend(Y,Z,W).

% The 'vanilla' meta-interpreter
solve( true  ).
solve( (A,B) ) :- solve(A),    solve(B).
solve( A     ) :- clause(A,B), solve(B).

% ?- solve(lappend([1,2],[3,4],L)).
% ?- solve(lappend(X,Y,[1,2,3,4])).

% A meta-interpreter that counts the number of (forward) steps
csolve( true,  0).
csolve( (A,B), N) :- csolve(A,NA), csolve(B,NB), N is NA+NB.
csolve( A,     N) :- clause(A,B),  csolve(B,N1), N is N1+1.

% ?- csolve(lappend([1,2],[3,4],L),N).
% ?- csolve(lappend([1,2,a,b],[3,4],L),N).
% ?- csolve(lappend([1,2],[3,4,a,b],L),N).

% -------------------------------------------------------------------
% Difference lists

dlist(X-Y) :- var(X), !, X == Y.
dlist([_|DL]-X) :- dlist(DL-X).

% Appending diference lists (in constant time):

append_dl(B1-E1,E1-E2,B1-E2).

% Queries:

% append_dl([1,2,3|X]-X,[4,5|Y]-Y,L).
% append_dl([1,2,3|X]-X,[4,5|Y]-Y,L-T).
% append_dl([1,2,3|X]-X,[4,5|Y]-Y,[1,2,3,4,5|Z]-Z).

% append_dl(L-X,[4,5|Y]-Y,[1,2,3,4,5|Z]-Z).
% append_dl(L1-X, L2-Y, [1,2,3,4,5|Z]-Z). % Only one solution!

% -------------------------------------------------------------------
% 

% -------------------------------------------------------------------
% Normal qsort

qsort([],[]).
qsort([X|L],S) :-
    partition(L,X,LS,LB),
    qsort(LS,LSS),
    qsort(LB,LBS), 
    append(LSS,[X|LBS],S).

partition([],_P,[],[]).
partition([E|R],P,[E|Left1],Right):- 
    E < P,
    partition(R,P,Left1,Right).
partition([E|R],P,Left,[E|Right1]):-
    E >= P,
    partition(R,P,Left,Right1).

% -------------------------------------------------------------------
% qsort with difference lists

% Partition is same as above!

%%% Version using -/ functor, explicit unifications

%% qsort_dl(L,SL) :-
%%     qsort_dl_(L,SL-SLE),
%%     SLE = [].
%% 
%% qsort_dl_([],SLE-SLE).
%% qsort_dl_([X|L],SL-SLE) :-
%%     partition(L,X,S,B),
%%     qsort_dl_(S,SS-SSE),
%%     qsort_dl_(B,BS-BSE),
%%     SSE = [X|BS],
%%     SL = SS,
%%     BSE = SLE.

% qsort_dl([5,2,1,9,7], SL).
% qsort_dl([5,1,7], SL).

%%% Version using -/ functor, implicit unifications

qsort_dl(L,SL) :-
    qsort_dl_(L,SL-[]).

    
qsort_dl_([],SLE-SLE).
qsort_dl_([X|L],SL-SLE) :-
    partition(L,X,S,B),
    qsort_dl_(S,SL-[X|BS]),
    qsort_dl_(B,BS-SLE).

%%% Version using extra arguments and explicit unifications

%% qsort_dl(L,SL) :-        % L  = [5,2,1,3,7,6]
%%     qsort_dl_(L,SL,SLE), % SL = [1,2,3,5,6,7|SLE]
%%     SLE = [].            % SL = [1,2,3,5,6,7]
%% 
%% qsort_dl_([],SLE,SLE).
%% qsort_dl_([X|L],SL,SLE) :-
%%     partition(L,X,S,B),  % S = [2,1,3], B = [7,6]
%%     qsort_dl_(S,SS,SSE), % SS = [1,2,3|SSE]
%%     qsort_dl_(B,BS,BSE), % BS = [6,7|BSE]
%%     SL = SS,
%%     SLE = BSE,
%%     SSE = [X|BS].        %  SSE=[5|BS]

% qsort_dl([5,2,1,3,7,6], SL).

%%% Version using extra arguments, implicit unifications

%% qsort_dl(L,SL) :-
%%     qsort_dl_(L,SL,[]).
%% 
%%     
%% qsort_dl_([],SLE,SLE).
%% qsort_dl_([X|L],SL,SLE) :-
%%     partition(L,X,S,B),
%%     qsort_dl_(S,SL,[X|BS]),
%%     qsort_dl_(B,BS,SLE).

% -------------------------------------------------------------------
% Parsing 'by hand' (append)

% ?- parse_app([t,h,e,' ',p,l,a,n,e,' ',f,l,i,e,s]).
% ?- parse_app(X).

parse_app(X) :-
    article_app(A),
    append(A,T1,X),
    spaces_app(S1),
    append(S1,T2,T1),
    noun_app(N),
    append(N,T3,T2),
    spaces_app(S2),
    append(S2,V,T3),
    verb_app(V).

article_app([a]).
article_app([t,h,e]).

spaces_app([' ']).
spaces_app([' ' | T]) :- 
    spaces_app(T).

noun_app([c,a,r]).
noun_app([p,l,a,n,e]).

verb_app([f,l,i,e,s]).
verb_app([d,r,i,v,e,s]).


% -------------------------------------------------------------------
% b) Parsing 'by hand' (difference lists)

% ?- parse_dl([t,h,e,' ',p,l,a,n,e,' ',f,l,i,e,s|Y],Y).
% ?- parse_dl(X,[]).

parse_dl(X,CV) :-
    article_dl(X,CA),
    spaces_dl(CA,CS1),
    noun_dl(CS1,CN),
    spaces_dl(CN,CS2),
    verb_dl(CS2,CV).

article_dl([a|X],X).
article_dl([t,h,e|X],X).

spaces_dl([' ' | X],X).
spaces_dl([' ' | Y],X) :- 
    spaces_dl(Y,X).

noun_dl([c,a,r | X],X).
noun_dl([p,l,a,n,e | X],X).

verb_dl([f,l,i,e,s | X],X).
verb_dl([d,r,i,v,e,s | X],X).

% -------------------------------------------------------------------
% Parsing: adding some syntax: strings

%% ?- parse_dls("the plane flies",[]).

parse_dls(X,CV) :-
    article_dls(X,CA),
    spaces_dls(CA,CS1), noun_dls(CS1,CN), 
    spaces_dls(CN,CS2), verb_dls(CS2,CV).

%% article_dls( [97|X], X).
%% article_dls( [0'a|X], X).
article_dls( "a"   || X, X).
article_dls( "the" || X, X).

spaces_dls( " "    || X, X).
spaces_dls( " "    || Y, X) :- spaces_dls(Y, X).

noun_dls( "car"    || X, X).
noun_dls( "plane"  || X, X).

verb_dls( "flies"  || X, X).
verb_dls( "drives" || X, X).

% -------------------------------------------------------------------
% Parsing: making the pair of "mechanical" arguments implicit -> DCGs

:- use_package(dcg).

%% ?- parse("the plane flies",[]).

%% parse(X,CV) :- article(X,CA), spaces(CA,CS1), noun(CS1,CN),
%                 spaces(CN,CS2), verb(CS2,CV).
% article( "a"   || X, X).


parse --> article, spaces, noun, spaces, verb.

article --> "a".
article --> "the".

spaces --> " ".
spaces --> " ", spaces.

noun --> "car".
noun --> "plane".

verb --> "flies".
verb --> "drives".

% -------------------------------------------------------------------
% Parsing: DCGs + Prolog

% ?- parse(NChars,"the plane flies",[]).

% parse(N,X1,X6) :- article(AC,X1,X2), spaces(S1,X2,X3), noun(NC,X3,X4), spaces(S2,X4,X5), verb(VC,X5,X6).

parse(N) --> article(AC), spaces(S1), noun(NC), spaces(S2), 
             verb(VC), { N is AC + S1 + NC + S2 + VC }.

article(1) --> "a".
article(3) --> "the".

spaces(1) --> " ".
spaces(N) --> " ", spaces(N1), { N is N1+1 }.

noun(3) --> "car".
noun(5) --> "plane".

verb(5) --> "flies".
verb(6) --> "drives".

% -------------------------------------------------------------------
% Other issues

foo :- repeat, read(X), process(X).

process(end).
process(X) :- display(X), fail.

% -------------------------------------------------------------------

wrepeat.
wrepeat :- wrepeat.

%% wrepeat, X=a.

%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

