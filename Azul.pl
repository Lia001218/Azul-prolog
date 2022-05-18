% :- module().
% se enumeran los jugadores , se les asigna una estrategia , zona de preparacion , murro y piso y se devuelven en una lista de 
% Players
create_players(Count_player, Players):-
    create_preparation_zone(Count_player,Preparation_zone),
    create_ground(Count_player,Ground),
    add([],Count_player,[Preparation_zone,Ground,[]:wall , 0:score],List),
    findall(P,(
        member(X,List),
        select_strategy(Strategy),
        assing(strategy,X,Strategy,P)
    ),Players_created),
    enumerate(Players_created,1,Players).


% Se crea la zona de preparacion , primero creamos una lista en la cual vamos a anadir en cada pocicion una lista de la siguiente manera
% [[],[],[],[],[]] , luego enumeramos cada elemento de esa lista(creamos 5 listas porque la zona de preparacion tiene 5 filas)
%luego se le asigna a cada pocision de la lista un size , los colores validos y todos loa colores , en un principio todos los colores,
% validos en cualquier fila, una vez asignado el size se crea una lista de lestas con el size correspondiente asignado, obteniendo lo 
% siguiente [[],[[],[]],[[],[],[]],[[],[],[],[]],[[],[],[],[],[]]]
create_preparation_zone(Count_player,Preparation_zone):-
    add([],5,1,List),
    enumerate(List,1,Enum),
    colors(C),
    findall(P,[New:zone, C : valid,C:all]:SZ,
        (member_dict(SZ,Enum,_),
        add([],SZ,empty,New))),
    Preparation_zone. 

strategies([basic,greedy]).

select_strategy(S):-
    strategies(Strategy),
    random_permutation(Strategy,[S|_]).
ejecute_round(OldRound,[],NewRound,[]).
ejecute_round(OldRound,[PlayerActual:Id|Players],NewRound,[Id:FacId|Accions]):-
    % Vemos cual es la estrategia del jugador actual
    member_dict(strategy,PlayerActual,St),
    % decimos quien juega ahora
    info_log(["Player" ,Id,"empieza el turno"]),
    %  Se ejecuta la estrategia del jugador actual
    Play =..[St,Game,PlayerActual,ActualRound,NewPlayer,LineId:FacId:Color],
    Play,
    info_log([
        "Player seleciono",
        Color ,
        "de",
        FacId ,
        "y lo coloca en linea",
        LineId]),
    % ve el estado de las factorias en la ronda actual
    member_dict(factories,ActualRound,Facs),
    debug_log([Facs:factories]),
    info_log([
        NewPreparation_zone]),
    % ve el tablero de los jugadores antes de ejecutar la jugada
    member_dict(players,ActualRound,OldBoard),
    % Actualiza el tablero
    assing(Id,OldBoard,NewPreparation_zone,ActualBoard),
    assing(players,ActualRound,ActualBoard,NextRound),
    % manda a jugar al proximo jugador
    ejecute_round(NextRound,Players,NewRound,Accions).
