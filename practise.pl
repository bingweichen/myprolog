%%%%%%%%%%%Q1   sublist(L1, L2)%%%%%%%%%%%
subList([],L2).
subList([H|Rest],L2):-
	check(H,L2),
	subList(Rest,L2).
check(H,[H2]):-
	H==H2.
check(H,[H1|Rest1]):-
	H\=H1,
	check(H,Rest1).
check(H,[H1|Rest1]):-
	H==H1.

/*
subList([],L2).
subList([H|Rest],L2):-
	member(H,L2),
	subList(Rest,L2).
*/
%%%%%%%%%%%Q2   difference(L1, L2, L)%%%%%%%%%%%


difference([], L2, []).
difference([H|Rest], L2, L):-
	member(H,L2),
	difference(Rest, L2, L).

difference([H|Rest], L2, L):-
	\+member(H,L2),
	append([H],Lsofar,L),
	difference(Rest, L2, Lsofar).

/*
difference([], L2, []).
difference([H|Rest], L2, L):-
	check(H,L2),
	difference(Rest,L2,L).

difference([H|Rest], L2, L):-
	write(H),nl,
	(check(H,L2)->
	difference(Rest,L2,L);
	 (append([H],Lsofar,L),
	  difference(Rest,L2,Lsofar))).
check1(H,[Hlast]):-
	H=Hlast.
check1(H,[H1,Rest1]):-
	H1\=H,
	check1(H,Rest1).
check1(H,[H1,Rest1]):-
	H1=H.
*/
%%%%%%%%%%%Q3   sift(L, N, Result)%%%%%%%%%%
sift([],N,[]).
sift([H|Rest],N,Result):-
	H=<N,
	append([H],Resultsofar,Result),
	sift(Rest,N,Resultsofar).
sift([H|Rest],N,Result):-
	H>N,
	sift(Rest,N,Result).
%%%%%%%%%%%Q4  common(L1, L2, I) %%%%%%%%%%
%common([],L2,[]).
common(L1,L2,I):-
	findall(H,(member(H,L1),member(H,L2)),Itemp),
	sort(Itemp,I).

%%%%%%%%%%%Q5 delete(L, Result) %%%%%%%%%%
delete([],[]).
delete([X,Y|Rest],Result):-
	append([X],Resultsofar,Result),
	delete(Rest,Resultsofar).
%%%%%%%%%%%Q6 process(L1, L2, Consistent, Inconsistent) %%%%%%%%%%
process(L1,[],[],[]).
process(L1,[(Name,Age,Marital_status)|Rest],Consistent, Inconsistent):-
	member((Name,Age),L1),
	append([(Name,Age,Marital_status)],Con_sofar,Consistent),
	process(L1,Rest,Con_sofar,Inconsistent).

process(L1,[(Name,Age,Marital_status)|Rest],Consistent, Inconsistent):-
	nonmember((Name,Age),L1),
	append([(Name,Age,Marital_status)],Incon_sofar,Inconsistent),
	process(L1,Rest,Consistent,Incon_sofar).
%%%%%%%%%%%Q7. split(L, N, L1, L2) %%%%%%%%%%
split(RestL,0,[],RestL).
split([H|Rest],N,L1,L2):-
	NewN is N-1,
	append([H],L1sofar,L1),
	split(Rest,NewN,L1sofar,L2).
%%%%%%%%%%%Q8. drop(L, N, Result) Drop) %%%%%%%%%%
drop(L1,N,L1):-
	length(L1,Length),
	Length<N.
drop(L1,N,Result):-
	length(L1,Length),
	Length>=N,
	myhelp(L1,N,Result_sofar,RestL),
	write(Result_sofar),nl,
	write(RestL),nl,
	append(Result_sofar,Resultsofar,Result),
	drop(RestL,N,Resultsofar).


myhelp([H|RestL],1,[],RestL).
myhelp([H|Rest],N,Result_sofar,RestL):-
	NewN is N-1,
	append([H],Resultsofar,Result_sofar),
	myhelp(Rest,NewN,Resultsofar,RestL).




/*drop([H|RestL],1,RestL).
drop([H|Rest],N,Result):-
	NewN is N-1,
	append([H],Resultsofar,Result),
	drop(Rest,NewN,Resultsofar).

*/

%%%%%%%%%%%Q9. enrolment(L, Student, Degree) %%%%%%%%%%
enrolment([],Student,Degree).
enrolment([(Degree,Namelist)|Rest],Student,Degree):-
	member(Student,Namelist).
enrolment([(Degree_of_this_list,Namelist)|Rest],Student,Degree):-
	nonmember(Student,Namelist),
	enrolment(Rest,Student,Degree).
%%%%%%%%%%%Q10. student_list(L, Meng, MSc) %%%%%%%%%%
student_list([],[],[]).
student_list([(Degree,Namelist)|Rest],Meng,MSc):-
	Degree=msc,
	append(Namelist,MScsofar,MSc),
	student_list(Rest,Meng,MScsofar).

student_list([(Degree,Namelist)|Rest],Meng,MSc):-
	Degree=meng,
	append(Namelist,Mengsofar,Meng),
	student_list(Rest,Mengsofar,MSc).
