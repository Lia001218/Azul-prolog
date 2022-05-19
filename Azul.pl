% se crea una lista de tamanno igual a la cantidad de jugadores,y se le annade a cada posicion (que cada posicion
% representa un jugador) la zona de preparacion y el piso,luego se le asigana una estrategia de forma ramdon a cada
% jugador y se crea una lista en la cual en la pocision i contando a partir de 1 va a estar la estrategia del jugador
% i,zona de preparacion del jugador i,piso del jugaor i, muro del jugador i y score del jugador i, de forma tal que 
% la posicion i de la lista tiene toda la informacion del jugador i
create_players(Count_player,Players:players):-
    create_preparation_zone(Preparation),
    create_ground(Ground),
    add([],Count_player,[Preparation,Ground,[]:wall , 0:score],List),
    findall(P,(
        member(X,List),
        select_strategy(Strategy),
        assing(strategy,X,Strategy,P)
    ),Player_created),
    enumerate(Player_created,1,Players).
create_ground([-1,-1,-2,-2,-2,-3,-3]:ground).
% primero se crea una lista de 5 elementos , luego se enumera de izquierda a derecha la lista , de forma tal que 
% en valor que le asigne el enum va a ser la cantidad de columnas que va a tener esa fila en la zona de preparacion
% una vez hecho esto teniendo en cuenta le numero que le asigan el enum a la posicion i , se utiliza el metoda add
% que crea una nueva lista a la cual le annade empty la cantidad de veces que diga el enum y con el metodo findall
% a su vez va creando una lista con dichas listas y ademas tambien guarda en cada pocicion i los colores que se 
% pueden poner en la lista i (en principio todos) y todos los colores posibles del juego
create_preparation_zone(Preparation:preparation):-
    add([],5,1,List),
    enumerate(List,1,Enum),
    colors(C),
    findall([New:zone, C : valid,C:all]:SZ,
        (member_dict(SZ,Enum,_),
        add([],SZ,empty,New)),
    Preparation). 
% assing es el encargado de asignar nuevos estados a los componentes del tablero,para ello necesita el componente 
% del tablero que se necesita modificar, lo que contiene dicho componente y el valor a actualizar en el mismo
assing(P,L,V,N):-
    remove(P,L,C),
    my_concat([V:P],C,N).
remove(_, [], []).
remove(P, [_:P|R], L) :- !,
    remove(P, R, L). 
remove(P, [X:Y|R], [X:Y|L]) :-
    Y\=P,
    remove(P, R, L).
my_concat([], X, X).
my_concat([X|R], Y, [X|Z]) :-
    my_concat(R, Y, Z).
% triunfa si el par V:P esta en el diccionario O
member_dict(P, O, V) :-
    member(V:P, O).
colors([red,blue,black,yellow,white]).
%  annade K veces el elemento X a L
add(L, 0, _, L).
add(L, K, X, R) :-
    K>0,
    P is K-1,
    my_concat(L, [X], L1),
    add(L1, P, X, R).

% asigna un numero a cada posicion de la lista
enumerate([], _, []).
enumerate([E1|List], Number, [E1:Number|Enum]) :-
    Next is Number+1,
    enumerate(List, Next, Enum).

strategies([basic,greedy]).
select_strategy(S):-
    strategies(St),
    random_permutation(St,[S|_]).
ejecute_round(OldRound,[],NewRound,[]).
ejecute_round(OldRound,[PlayerActual:Id|Players],NewRound,[Id:FacId|Accions]):-
    % Vemos cual es la estrategia del jugador actual
    member_dict(strategy,PlayerActual,St),
    % decimos quien juega ahora
    % info_log(["Player" ,Id,"empieza el turno"]),
    %  Se ejecuta la estrategia del jugador actual
    Play =..[St,Game,PlayerActual,ActualRound,NewPlayer,LineId:FacId:Color],
    Play,
    % info_log([
    %     "Player seleciono",
    %     Color ,
    %     "de",
    %     FacId ,
    %     "y lo coloca en linea",
    %     LineId]),
    % ve el estado de las factorias en la ronda actual
    member_dict(factories,ActualRound,Facs),
    % debug_log([Facs:factories]),
    % info_log([
    %     NewPreparation_zone]),
    % ve el tablero de los jugadores antes de ejecutar la jugada
    member_dict(players,ActualRound,OldBoard),
    % Actualiza el tablero
    assing(Id,OldBoard,NewPreparation_zone,ActualBoard),
    assing(players,ActualRound,ActualBoard,NextRound),
    % manda a jugar al proximo jugador
    ejecute_round(NextRound,Players,NewRound,Accions).
