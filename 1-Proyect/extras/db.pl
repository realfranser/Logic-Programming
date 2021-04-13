:- module(_, _, [classic]).

loves(romeo, juliet).
loves(juliet, romeo) :- loves(romeo, juliet).

male(albert).
male(bob).
male(bill).
male(carl).
male(charlie).
male(dan).
male(edward).

female(alice).
female(betsy).
female(diana).
female(mia).


parent(albert, bob).
parent(albert, betsy).
parent(albert, bill).

parent(bob, charlie).
parent(bob, carl).

get_grandchild(Y) :-
  parent(albert, X),
  parent(X, Y),
  format('~w ~s grandparent ~n', [X, "is the"]).


related(X, Y) :-
  parent(X, Y).

related(X, Y) :-
  parent(X, Z),
  related(Z, Y).



what_grade(5) :-
  write('Go to kindergarten').

what_grade(6) :-
  write('Go to 1st Grade').

what_grade(Other) :-
  Grade is Other - 5,
  format('Go to grade ~w', [Grade]).

owns(albert, pet(cat, olive)).

customer(tom, smith, 20.55).
customer(sally, smith, 120.55).

get_customer_bal(FName, LName) :-
  customer(FName, LName, Bal),
  write(FName), tab(1),
  format('~w owes us $~2f ~n', [LName, Bal]).

warm_blooded(penguin).
warm_blooded(human).

produce_milk(penguin).
produce_milk(human).

have_feathers(penguin).
have_hair(human).

mammal(X) :-
  warm_blooded(X),
  produce_milk(X),
  have_hair(X).

double_digit(X, Y) :-
  Y is X*2.
is_even(X) :-
  Y is X//2, X =:= 2*Y.

% Read imput from user
say_hi :-
  write('What is your name? '),
  read(X),
  write('Hi '),
  write(X).

% Looping

count_to_10(10) :-
  write(10), nl.

count_to_10(X) :-
  write(X), nl,
  Y is X + 1,
  count_to_10(Y).

count_down(Low, High, Z) :-
  between(Low, High, Y),
  Z is High - Y,
  write(Z), nl.

count_up(Low, High, Z) :-
  between(Low, High, Y),
  Z is Y + Low,
  write(Z), nl.


