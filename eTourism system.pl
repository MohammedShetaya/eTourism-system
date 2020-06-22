
%offer


% offerMean(X, Y) -> Transportation mean Y is used with offer X

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4),bus).

% offerAccommodation(X, Y) -> Accommodation Y is part of offer X

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4),hotel).

%CustomerPreferredActivity(X, Y, R) -> Y is the preferred activity kind wrt customer X with relevance R

customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving , 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50).

% customerPreferredMean(X, Y, R) -> Y is the preferred transportaion mean wrt customer X with relevance R

customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), plane, 10).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).

% customerPreferredAccommodation(X, Y, R) -> Y is the preferred accommodation to customer X with relevance R

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Predicate 1
possibleSubset(L,R):-possibleSubset(L,[],R).

possibleSubset([],Acc,R):-perm(Acc,R).
possibleSubset([H|T],Acc,R):-possibleSubset(T,[H|Acc],R).
possibleSubset([H|T],Acc,R):-possibleSubset(T,Acc,R).


perm([X|Y],Z) :- perm(Y,W), remove2(X,Z,W).
perm([],[]).

remove2(X,[X|R],R).
remove2(X,[F|R],[F|S]) :-
                    X\=F,
                    remove2(X,R,S).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Predicate 2

choosePreferences(L,R):-
                    find_activity(L,Q),
                    Q = activity(X),
                    perm_activity(activity(X),Z),
                    remove_activity(L,[],S),
                    possibleSubset([activity(Z)|S],R).
choosePreferences(L,R):-
                    find_activity(L,Q),
                    Q = [],
                    possibleSubset(L,R).

remove_activity([],Acc,Acc). 
 
remove_activity([H|T],Acc,R):-
                   H = activity(X),
                   remove_activity(T,Acc,R).

remove_activity([H|T],Acc,R):-
                   H \= activity(X),
                   remove_activity(T,[H|Acc],R).

find_activity([],[]).
find_activity(activity(X),activity(X)).
find_activity([H|T],R):- 
               H = activity(X),
              find_activity(activity(X),R).
find_activity([H|T],R):-
              H \= activity(X) ,
              find_activity(T,R). 

perm_activity(activity(X),R):-possibleSubset(X,R).     
              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% predicate 3

preferenceSatisfaction(offer(A,B,C,D,E,F,G,H),Customer,ChosenPrefs, S):-
offerAccommodation(offer(A,B,C,D,E,F,G,H),Acc),
offerMean(offer(A,B,C,D,E,F,G,H),Mean),
O = [Acc,Mean|B],
lol(ChosenPrefs,Pref),
lol1(O,Pref,Common),
lol2(Customer,Common,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lol([],[]).
lol([H|T],X):-
H=activity(A),lol(T,T1),append(A,T1,X).
lol([accommodation(A)|T],[A|T1]):-
lol(T,T1).
lol([means(A)|T],[A|T1]):-
lol(T,T1).
lol([H|T],X):-
H \= activity(A),H \= accommodation(A) , H \= means(A),lol(T,X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lol1(_,[],[]).
lol1(O,[H|T],[H|T1]):-
member(H,O),lol1(O,T,T1).
lol1(O,[H|T],X):-
\+ member(H,O),lol1(O,T,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lol2(_,[],0).
lol2(Customer,[H|T],S):-
((customerPreferredActivity(Customer,H,S1);customerPreferredMean(Customer,H,S1);customerPreferredAccommodation(Customer,H,S1)),
lol2(Customer,T,S2),S is S1+S2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% predicate 4

comes_first(A1-A2-A3,B1-B2-B3):-
                     (A1<B1);((A1=B1),(A2<B2));((A1=B1),(A2=B2),(A3<B3)).    

overlapPeriod(period(A1-A2-A3,B1-B2-B3),period(C1-C2-C3,D1-D2-D3)):-
(comes_first(A1-A2-A3,C1-C2-C3),lapPeriod(period(A1-A2-A3,B1-B2-B3),period(C1-C2-C3,D1-D2-D3)));
(comes_first(C1-C2-C3,A1-A2-A3),lapPeriod(period(C1-C2-C3,D1-D2-D3),period(A1-A2-A3,B1-B2-B3))).
                
lapPeriod(period(A1-A2-A3,B1-B2-B3),period(C1-C2-C3,D1-D2-D3)):-
(((B1\=C1),(B1>C1)) ;  ((B1=C1),(B2>C2))  ; ((B1=C1),(B2=C2),(B3>=C3))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% predicate 5

getOffer(L,R):-
             setof(X,offerMean(X,_),Off),
             looping(L,Off,R),
             R  = offer(A,B,C,D,E,F,G,H) .

                        
looping([A|B],[H|T],R):-
       loop_test([A|B],H),
       ((R=H) ; looping([A|B],T,R)).

looping([A|B],[H|T],R):-
            (\+ loop_test([A|B],H)),
            looping([A|B],T,R). 

             
loop_test([],_).         
loop_test([H|T],Off):-
             test(Off,H),
             loop_test(T,Off).

test(Off,dest(X)):- Off = offer(X,_,_,_,_,_,_,_) .
test(Off,budget(X)):- Off = offer(_,_,R,_,_,_,_,_) , X>=R .
test(Off,activity(X)):- Off = offer(_,Y,_,_,_,_,_,_),memo(X,Y).
test(Off,period(X,Y)):- Off = offer(_,_,_,_,_,period(W,Z),_,_) , overlapPeriod(period(X,Y),period(W,Z)).
test(Off,means(X)):- offerMean(Off,X).
test(Off,accommodation(X)):- offerAccommodation(Off,X).


memo([],_).
memo([H|T],L):-
         memoo(H,L),
         memo(T,L).
     
memoo(A,L):- member(A,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% predicate 6

recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
possibleSubset(Prefs, ChosenPrefs),
getOffer(ChosenPrefs,O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% predicate 7
calculateSatisfactionForCustomers([], [], _, []).
calculateSatisfactionForCustomers([Customer|T], [CustomerPrefs|T1], Offer, [(Sat, Customer)|T2]):-
        preferenceSatisfaction(Offer, Customer, CustomerPrefs, Sat),
        calculateSatisfactionForCustomers(T, T1, Offer, T2).

chooseNCustomers(_, 0, []).
chooseNCustomers([], _, []).
chooseNCustomers([(S, H)|T], NoOfGuests, [H|T1]):-
        NoOfGuests > 0,
        N1 is NoOfGuests - 1,
        chooseNCustomers(T, N1, T1). 

recommendOffer(Customers, PreferenceList, Offer, CustomersChosen):-
        offerMean(Offer, _),
        calculateSatisfactionForCustomers(Customers, PreferenceList, Offer, Satisfactions),
        sort(0,@>=,Satisfactions, S),
        Offer = offer(_, _, _, _, _, _, _, NoOfGuests),
        chooseNCustomers(S, NoOfGuests, CustomersChosen).	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




	