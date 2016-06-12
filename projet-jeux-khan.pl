%---------Fonctions outils

%Donne la valeur du n e element d'une liste
nElement(1,[T|Q],T) :- !.
nElement(N,[T|Q],X) :- 
		N>1,
		Nbis is N-1,
		nElement(Nbis, Q, X). 

%vrai si l'element X appartient à la liste 
element([T|Q],T) :- !.
element([T|Q],X) :- 
		element(Q, X).

%retourne le rang de l'element X dans la liste passé en deuxième argument
rang(X,[X|Q],1):-!.
rang(X,[_|Q],N):-rang(X,Q,L),N is L+1.

%concaténe deux listes dans la troisième
concat([],L,L) :- !.
concat([T|Q],L,[T|R]) :- concat(Q,L,R).

%insert X à la Nieme position de la première liste et rtourne le résultat dans la deuxieme 
insertionN(X,L,[X|L],1) :- !.
insertionN(X,[Y|L1],[Y|L2],N) :- 
		N>1,
		Nbis is N-1,
		insertionN(X,L1,L2,Nbis),
		nElement(Nbis,L2,X).

%retire le Ne element de la liste
retirerNeElement(N,[],[]).
retirerNeElement(1,[X|Q],Q) :- !.
retirerNeElement(N,[T|Q],[T|R]) :- 
		N>1,
		Nbis is N-1,
		retirerNeElement(Nbis,Q,R).
								   
%affiche une liste								   
afficheListe([]).
afficheListe([T|Q]) :- write(T), tab(1), afficheListe(Q).

%je comprend pas ce que c'est
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

%------------ Variable dynamique pour la grille de jeu : sert à l affichage

%grille des valeurs
:- dynamic(grille/1).
grille(	[2,3,1,2,2,3,
		2,1,3,1,3,1,
		1,3,2,3,1,2,
		3,1,2,1,3,2,
		2,3,1,3,1,3,
		2,1,3,2,2,1]).

replaceGrille(L) :- grille(X),retract(grille(X)), asserta(grille(L)).

%grille des positions
:- dynamic(lposition/1).
lposition(	[n,n,n,n,n,n,
			n,n,n,n,n,n,
			n,n,n,n,n,n,
			n,n,n,n,n,n,
			n,n,n,n,n,n,
			n,n,n,n,n,n]).

replaceLposition(L) :- lposition(X),retract(lposition(X)), asserta(lposition(L)).

%position du Khan
:- dynamic(khan/1).
khan([]).
replaceKhan(L):- khan(X),retract(khan(X)), asserta(khan(L)).

%listes des pions en vie et des pions morts
:- dynamic(pionRougeEnVie/1).
pionRougeEnVie([kr, s1r, s2r, s3r, s4r, s5r]).
:- dynamic(pionRougeMort/1).
pionRougeMort([]).
:- dynamic(pionOcreEnVie/1).
pionOcreEnVie([ko, s1o, s2o, s3o, s4o, s5o]).
:- dynamic(pionOcreMort/1).
pionOcreMort([]).
			
%affichage du plateau pour le choix du coté
affichageBoardChoix(Board) :-
		write('              1            '),
		nl,
		write('  | 2 | 3 | 1 | 2 | 2 | 3 |'),
		nl,
		write('  | 2 | 1 | 3 | 1 | 3 | 1 |'),
		 nl,
		write('4 | 1 | 3 | 2 | 3 | 1 | 2 | 2'),
		nl,
	 	write('  | 3 | 1 | 2 | 1 | 3 | 2 |'),
		nl,
		write('  | 2 | 3 | 1 | 3 | 1 | 3 |'),
		nl,
	 	write('  | 2 | 1 | 3 | 2 | 2 | 1 |'),
		nl,
		write('              3            '),
	 	nl.

%initialisaion du plateau selon le choix des joueurs
initTab(1,Tab) :- 
		Tab  = [2,3,1,2,2,3,
		 	    2,1,3,1,3,1,
				1,3,2,3,1,2,
				3,1,2,1,3,2,
				2,3,1,3,1,3,
				2,1,3,2,2,1],
		replaceGrille(Tab).

initTab(2,Tab) :- 
		Tab  = [3,1,2,2,3,1,
			    2,3,1,3,1,2,
				2,1,3,1,3,2,
				1,3,2,2,1,3,
				3,1,3,1,3,1,
				2,2,1,3,2,2],
		replaceGrille(Tab).
					   
initTab(3,Tab) :- 
		Tab  = [1,2,2,3,1,2,
		        3,1,3,1,3,2,
				2,3,1,2,1,3,
				2,1,3,2,3,1,
				1,3,1,3,1,2,
				3,2,2,1,3,2],
		replaceGrille(Tab).
					   
initTab(4,Tab) :- 
		Tab  = [2,2,3,1,2,2,
				1,3,1,3,1,3,
				3,1,2,2,3,1,
			    2,3,1,3,1,2,
				2,1,3,1,3,2,
				1,3,2,2,1,3],
		replaceGrille(Tab).

%--------------Fonctions d affichages du plateau : pour afficher le plateau utiliser afficherJeu(Argument_Quelconque)--------------
afficherPlateau([],[]) :- !.
afficherPlateau([T1|[T2|[T3|[T4|[T5|[T6|Q1]]]]]], [H1|[H2|[H3|[H4|[H5|[H6|Q2]]]]]]) :- 
		write(' ----------------------- '),nl,
		format("|~w  |~w  |~w  |~w  |~w  |~w  |",[T1,T2,T3,T4,T5,T6]),nl,
		afficherPion(H1),afficherPion(H2),afficherPion(H3),afficherPion(H4),afficherPion(H5),afficherPion(H6), write('|'),nl,
		write(' ----------------------- '),nl,
		afficherPlateau(Q1,Q2).



afficherPion(n) :- write('|   ').
afficherPion(kr) :- write('| KR').
afficherPion(ko) :- write('| KO').
afficherPion(s1r) :- write('| X ').
afficherPion(s2r) :- write('| X ').
afficherPion(s3r) :- write('| X ').
afficherPion(s4r) :- write('| X ').
afficherPion(s5r) :- write('| X ').
afficherPion(s6r) :- write('| X ').
afficherPion(s1o) :- write('| O ').
afficherPion(s2o) :- write('| O ').
afficherPion(s3o) :- write('| O ').
afficherPion(s4o) :- write('| O ').
afficherPion(s5o) :- write('| O ').
afficherPion(s6o) :- write('| O ').


afficherJeu(Arg) :- write(' -1---2---3---4---5---6- '),nl,
		lposition(L2),grille(L1),
		afficherPlateau(L1, L2).
%--------------------------------------------------------------------------					   

%rafraichit la liste lposition donnant la position de chaque pion 
rafraichirPosition(Nom,[X,Y]) :- 
		N is (Y-1)*6+X,
		lposition(L1),
		nElement(N,L1,n),
		replace(L1,N,Nom,L2),
		replaceLposition(L2).

%permet de placer tous un pion rouge en début de partie
placerPionR(NomPion) :-  
		write(NomPion), tab(1),
		write('V : '),
		read(X),
		integer(X),
		X =< 6,
		X >= 1,
		nl,
		write('H : '),
		read(Y),
		integer(Y),
		Y =< 2,
		Y >= 1,
		nl,
		Id=[X,Y],
		rafraichirPosition(NomPion, Id),!.

placerPionR(X) :- placerPionR(X).

%permet de placer tous un pion ocre en début de partie
placerPionO(NomPion) :-  
		write(NomPion), tab(1),
		write('V : '),
		read(X),
		integer(X),
		X =< 6,
		X >= 1,
		nl,
		write('H : '),
		read(Y),
		integer(Y),
		Y =< 6,
		Y >= 5,
		nl,
		Id=[X,Y],
		rafraichirPosition(NomPion, Id),!.

placerPionO(X) :- placerPionO(X).										   
									
%permet de placer tous les pions en début de partie
positionnerPion(X):- 
		write('Rouge, placez vos pions'),
		nl,
		placerPionR(kr),
		afficherJeu(Arg),
		placerPionR(s1r),
		afficherJeu(Arg),
		placerPionR(s2r),
		afficherJeu(Arg),
		placerPionR(s3r),
		afficherJeu(Arg),
		placerPionR(s4r),
		afficherJeu(Arg),
		placerPionR(s5r),
		afficherJeu(Arg),
		write('Ocre, placez vos pions'),
		nl,
		placerPionO(ko),
		afficherJeu(Arg),
		placerPionO(s1o),
		afficherJeu(Arg),
		placerPionO(s2o),
		afficherJeu(Arg),
		placerPionO(s3o),
		afficherJeu(Arg),
		placerPionO(s4o),
		afficherJeu(Arg),
		placerPionO(s5o),
		afficherJeu(Arg).


%retourne la position d'un pion
positionPion(Nom,L,[X,Y]) :- 
		rang(Nom,L,N),
		X is N mod 6,
		Y is (N//6)+1.

nomPion(Nom,L,[X,Y]) :- 
		N is (Y-1)*6+X,
		nElement(N,L,Nom).

%retourne la valeur d'une position
valeurPosition([X,Y],V):- 
		N is (Y-1)*6+X, 
		grille(Board),
		nElement(N,Board,V).

%retourne la valeur du Khan
valeurKhan(0):- khan([]),!.
valeurKhan(V):- 
		khan([X,Y]),
		grille(Board),
		N is (Y-1)*6+X, 
		nElement(N,Board,V),
		write(V),!.

%retourne la valeur d'un pion
valeurPion(Nom,V) :-	
		lposition(L),
		positionPion(Nom,L,P),
		valeurPosition(P,V).

%retourne la liste des pions ayant la meme valeur que le Khan
comparaisonVKhan([],_,[]).
comparaisonVKhan(L,0,L):-!.%si le Khan n'est pas initialiser alors on peut déplacer tout les pions
comparaisonVKhan([Nom|Q],VKhan,LPion) :- 
		comparaisonVKhan(Q,VKhan,L1),
		valeurPion(Nom,V),
		V=VKhan,
		LPion=[Nom|L1],!.
comparaisonVKhan([Nom|Q],VKhan,LPion) :- 
		comparaisonVKhan(Q,VKhan,L1),
		valeurPion(Nom,V),
		V\=VKhan,
		LPion=L1,!.

%retourne la liste des pions pouvant bouger
pionPouvantBouger(PionList, Board, ocre,VKhan) :- 
		pionOcreEnVie(PionOcre),
		comparaisonVKhan(PionOcre,VKhan,PionList).
pionPouvantBouger(PionList, Board, rouge, VKhan) :- 
		pionRougeEnVie(PionRouge),
		comparaisonVKhan(PionRouge,VKhan,PionList).

%mouvements si khan ne bloque aucun pion
listePion([], Board, ocre, PionRessucite,V) :- 
		replaceKhan([]),
		pionOcreMort(M),
		M \= [],
		write('voulez-vous ressuciter(0) un pion ou jouer un pion(1) '),
		nl,
		read(Action),
		integer(Action),
		Action = 0,
		ressuciterPion(M,PionRessucite),
		valeurPion(PionRessucite,V),!.
listePion([], Board, rouge, PionRessucite, V) :- 
		replaceKhan([]),
		pionRougeMort(M),
		M \= [],
		write('voulez-vous ressuciter(0) un pion ou jouer un pion(1) '),
		nl,
		read(Action),
		integer(Action),
		Action = 0,
		ressuciterPion(M,PionRessucite,V),!.

listePion([], Board, ocre, PionPouvantBouger,V) :- 
		replaceKhan([]),
		pionOcreMort(M),
		M \= [],
		write('voulez-vous ressuciter(0) un pion ou jouer un pion(1) '),
		nl,
		read(Action),
		integer(Action),
		Action = 1,
		pionOcreEnVie(PionPouvantBouger),
		valeurPion(PionRessucite,V),!.
listePion([], Board, rouge, PionPouvantBouger,V) :- 
		replaceKhan([]),
		pionRougeMort(M),
		M \= [],
		write('voulez-vous ressuciter(0) un pion ou jouer un pion(1) '),
		nl,
		read(Action),
		integer(Action),
		Action = 1,
		pionRougeEnVie(PionPouvantBouger),
		valeurPion(PionRessucite,V),!.

%mouvements si liste des pions pouvant bouger est non vide.
listePion(L,_,_,L,0):- L\=[].

%vérifie si une position est valide ou non
verifierPosition([X,Y],Lposition,Player,Nom):-
		X>0,
		X<7,
		Y>0,
		Y<7,
		N is X+(Y-1)*6,
		nElement(N,Lposition,n),!.
verifierPosition([X,Y],Lposition,Player,Nom):-
		X>0,
		X<7,
		Y>0,
		Y<7,
		N is X+(Y-1)*6,
		nElement(N,Lposition,Nom),!.

%vérifie si une position finale est valide
verifierPosition0([X,Y],Lposition,Player,Nom):-
		X>0,
		X<7,
		Y>0,
		Y<7,
		N is X+(Y-1)*6,
		nElement(N,Lposition,n),!.
verifierPosition0([X,Y],Lposition,Player,Nom):-
		X>0,
		X<7,
		Y>0,
		Y<7,
		N is X+(Y-1)*6,
		nElement(N,Lposition,Nom),!.
verifierPosition0([X,Y],Lposition,rouge,Nom):-
		X>0,
		X<7,
		Y>0,
		Y<7,
		N is X+(Y-1)*6,
		nElement(N,Lposition,Po),
		pionOcreEnVie(Lpo),
		element(Lpo,Po),!.
verifierPosition0([X,Y],Lposition,ocre,Nom):-
		X>0,
		X<7,
		Y>0,
		Y<7,		
		N is X+(Y-1)*6,
		nElement(N,Lposition,Pr),
		pionRougeEnVie(Lpr),
		element(Lpr,Pr),!.

%parcours les différents chemins possibles
parcourirChemins(0,[X,Y],[[X,Y]],Lposition,Player,T):-
		verifierPosition0([X,Y],Lposition,Player,T),!.
parcourirChemins(N,[X,Y],L,Lposition,Player,T):-
		verifierPosition([X,Y],Lposition,Player,T),
		N > 0,
		M is N-1,
		X1 is X-1,
		X2 is X+1,
		Y1 is Y-1,
		Y2 is Y+1,
		parcourirChemins(M,[X1,Y],Res1,Lposition,Player,T),
		parcourirChemins(M,[X,Y1],Res2,Lposition,Player,T),
		parcourirChemins(M,[X,Y2],Res3,Lposition,Player,T),
		parcourirChemins(M,[X2,Y],Res4,Lposition,Player,T),
		append(Res1,Res2,L1),
		append(L1,Res3,L2),
		append(L2,Res4,L),!.
parcourirChemins(_,[_,_],[],_,_,_):-!.
% cas pour le premier déplacement
parcourirChemins1(N,[X,Y],L,Lposition,Player,T):-
		N > 0,
		M is N-1,
		X1 is X-1,
		X2 is X+1,
		Y1 is Y-1,
		Y2 is Y+1,
		parcourirChemins(M,[X1,Y],Res1,Lposition,Player,T),
		parcourirChemins(M,[X,Y1],Res2,Lposition,Player,T),
		parcourirChemins(M,[X,Y2],Res3,Lposition,Player,T),
		parcourirChemins(M,[X2,Y],Res4,Lposition,Player,T),
		append(Res1,Res2,L1),
		append(L1,Res3,L2),
		append(L2,Res4,L),!.

%concaténe les mouvements possibles 
typeMouvementPossible([],[],Player).
typeMouvementPossible([T|Q],[[T|Lmovements]|Suite],Player):-
		lposition(L),
		positionPion(T,L,P),
		valeurPion(T,V),
		parcourirChemins1(V,P,Lmovements,L,Player,T),
		typeMouvementPossible(Q,Suite,Player).

%retourne la liste des mouvements possibles
possibleMoves(Board,Player,PossibleMoveList) :- 
		valeurKhan(VKhan),
		pionPouvantBouger(PionList, Board, Player, VKhan),
		listePion(PionList,Board,Player,Pion,V),
		typeMouvementPossible(Pion,PossibleMoveList,Player).

%vérifie que le pion est bien le pion du joueur
verifierPionSelectionR(kr) :-!.
verifierPionSelectionR(s1r):-!.
verifierPionSelectionR(s2r):-!.
verifierPionSelectionR(s3r):-!.
verifierPionSelectionR(s4r):-!.
verifierPionSelectionR(s5r):-!.
verifierPionSelectionR(X) :- write('Pion selectioné invalide ! '),nl, fail.
verifierPionSelectionO(ko):-!.
verifierPionSelectionO(s1o):-!.
verifierPionSelectionO(s2o):-!.
verifierPionSelectionO(s3o):-!.
verifierPionSelectionO(s4o):-!.
verifierPionSelectionO(s5o):-!.
verifierPionSelectionO(X) :- write('Pion selectioné invalide ! '),nl, fail.

% vrai quand un joueur a gagné
gagne(ocre):-
		pionRougeMort(L),
		element(kr,L),!.

gagne(rouge):-
		pionOcreMort(L),
		element(ko,L),!.

%retourne les mouvements possibles associés au pion T
mouvementPion(T,[T|M],M).

% retourne la listes des positions possible pour un pion
parcoursMouvements([],_,[]).
parcoursMouvements([T|_],NomPion,Mouvements):-
		mouvementPion(NomPion,T,Mouvements),!.
parcoursMouvements([_|Q],NomPion,M):-
		parcoursMouvements(Q,NomPion,M).

%vérifies si un déplacement est possible
verifierDeplacement(Nom,Position,Player):-
		possibleMoves(Board,Player,Lmoves),
		parcoursMouvements(Lmoves,Nom,LmovePion),
		afficheListe(LmovePion),
		element(LmovePion,Position).

% rafrachit la position après un coup					   
rafraichirPositionCoup(Nom,[X,Y]) :- 
		N is (Y-1)*6+X,
		lposition(L1),
		replace(L1,N,Nom,L2),
		replaceLposition(L2).

% joue un coup pour rouge
jouerCoupR([X,Y]) :- 	
		lposition(L),
		nomPion(NomPion,L,[X,Y]),
		verifierPionSelectionR(NomPion),
		write('Nouvelle position V :  '), read(NewX), integer(NewX),
		write('Nouvelle position H :  '), read(NewY), integer(NewY),
		%%trace,
		verifierDeplacement(NomPion,[NewX,NewY],rouge),
	    rafraichirPositionCoup(n, [X,Y]),
		rafraichirPositionCoup(NomPion, [NewX,NewY]),
		%%notrace,
		nl,write('avant changement khan'),nl,
		replaceKhan([NewX,NewY]),
		write('apres changement khan'),!.	

jouerCoupR(X) :- write('mouvement impossible, rejouez'),nl,jouerRouge(X).

jouerRouge(Arg) :-
		khan([]),	
		write('Rouge: '),nl,
		write('Position V du pion à bouger : '), read(X), integer(X),
		write('Position H du pion à bouger : '), read(Y), integer(Y),
		jouerCoupR([X,Y]),!.

jouerRouge(Arg) :-	
		valeurKhan(V),
		write('Rouge: '),nl,
		write('Position V du pion à bouger : '), read(X), integer(X),
		write('Position H du pion à bouger : '), read(Y), integer(Y),
		valeurPosition([X,Y],V),
		jouerCoupR([X,Y]),!.

jouerRouge(Arg) :- write('Pion non valide veuilliez jouer un autre pion'),jouerRouge(Arg).

% joue un coup pour orcre
jouerCoupO([X,Y]) :- 	
		lposition(L),
		nomPion(NomPion,L,[X,Y]),
		verifierPionSelectionO(NomPion),
		write('Nouvelle position V :  '), read(NewX), integer(NewX),
		write('Nouvelle position H :  '), read(NewY), integer(NewY),
		verifierDeplacement(NomPion,[NewX,NewY],ocre),
	    rafraichirPositionCoup(n, [X,Y]),
		rafraichirPositionCoup(NomPion, [NewX,NewY]),
		replaceKhan([NewX,NewY]),!.

jouerCoup0(X) :- write('mouvement impossible, rejouez'),nl,jouerOcre(X).		     

jouerOcre(Arg) :-
		valeurKhan(V),
		write([V]), nl,
		write('Ocre :'),nl,	
		write('Position V du pion à bouger : '), read(X), integer(X),
		write('Position H du pion à bouger : '), read(Y), integer(Y),
		lposition(L),
		valeurPosition([X,Y],V),
		jouerCoupO([X,Y]),!.
		

jouerOcre(Arg) :- write('Pion non valide veuilliez jouer un autre pion'),jouerOcre(Arg).

% déroulement d'un tour
tour(Arg) :-gagne(rouge), write('victoire de rouge'),!.
tour(Arg) :-gagne(ocre),write('victoire de ocre'),!.
tour(Arg):-
		afficherJeu(Arg),
		jouerRouge(Arg),
		\+gagne(rouge),
		afficherJeu(Arg),
		jouerOcre(Arg),
		\+gagne(ocre),
		tour(Arg).

%-----------Règles pour l'IA----------------------
placerKo_IA([X,Y]) :- rafraichirPositionCoup(ko,[X,6]).
%-------------------- Dans les prédicats suivants les arguments X et Y correspondent aux positions de la kalista et non pas des pions.
/*-------------------------------
On place les pions à gauche et en haut à gauche
--------------------------------*/
placerSo_IA(NomPion,[X,Y]) :- lposition(L),  X > 1, NextX is X-1, N is NextX+(Y-1)*6, nElement(N,L,n),rafraichirPositionCoup(NomPion,[NextX,Y]).
%--placerSo_IA(NomPion,[X,Y]) :- lposition(L),  X > 1, NextX is X-1, NextY is Y-1, N is NextX+(NextY-1)*6, nElement(N,L,n),rafraichirPositionCoup(NomPion,[NextX,NextY]).
/*-------------------------------
On place les pions à droite et en haut à droite
--------------------------------*/
placerSo_IA(NomPion,[X,Y]) :- lposition(L), X < 6, NextX is X+1, N is NextX+(Y-1)*6, nElement(N,L,n),rafraichirPositionCoup(NomPion,[NextX,Y]).
%--placerSo_IA(NomPion,[X,Y]) :- lposition(L), X < 6, NextX is X+1, NextY is Y-1, N is NextX+(NextY-1)*6, nElement(N,L,n),rafraichirPositionCoup(NomPion,[NextX,NextY]).
/*-------------------------------
On place un pion devant
--------------------------------*/
placerSo_IA(NomPion,[X,Y]) :- lposition(L),NextY is Y-1, N is X+(NextY-1)*6, nElement(N,L,n),write(N), nl,rafraichirPositionCoup(NomPion,[X,NextY]).
/*-------------------------------
Cas Particuliers : On effectue un placement random
--------------------------------*/
placerSo_IA(NomPion,[X,Y]) :- lposition(L), random(1,7,NextX),random(5,7,NextY),CheckX is X+1, NextX\=CheckX,CheckX2 is X-1, NextX\=CheckX2, N is NextX+(NextY-1)*6,nl,nElement(N,L,n),rafraichirPositionCoup(NomPion,[NextX,NextY]).
placerSo_IA(NomPion,[X,Y]) :- placerSo_IA(NomPion,[X,Y]).

placerAll_IA(Arg) :-	random(1,7,X),
			placerKo_IA([X,6]),
			placerSo_IA(s1o,[X,6]),
			placerSo_IA(s2o,[X,6]),
			placerSo_IA(s3o,[X,6]),
			placerSo_IA(s4o,[X,6]),
			placerSo_IA(s5o,[X,6]).

%------------------------------------------------

%initialise le plateau
initBoard(Board):-
		replaceKhan([]),
		replaceLposition([n,n,n,n,n,n,
						n,n,n,n,n,n,
						n,n,n,n,n,n,
						n,n,n,n,n,n,
						n,n,n,n,n,n,
						n,n,n,n,n,n]),
		affichageBoardChoix(Board),
		%% write('Choix du cote du plateau'),
		%% nl,
		%% read(X),
		%% integer(X),
		%% open('camera.txt',write,Stream),
  %%        	write(Stream,X),  nl(Stream),
  %%        	close(Stream),
		initTab(X,Tab),
		%----------Premier Affichage------------------------
		grille(L),
		replaceGrille(Tab),
		afficherJeu(Arg).

initPionH_H(Board) :- positionnerPion(X).
					   
initPionIA_H(Board) :- 
		positionnerPion(X),
		placerAll_IA(Arg),afficherJeu(Arg).

%jeu humain contre humain
jeux(0):-
		initBoard(Board),
		initPionH_H(Board),
		tour(Board),!.
%si choix mal fait
jeux(X):- write('choix non valide'),nl,lancerJeux(T).

% choix du mode jeu
lancerJeux(Board):-
		write('voulez-vous jouer humain contre humain (0), humain contre IA (1) et IA contre IA (2)'),
		read(X),
		jeux(X).