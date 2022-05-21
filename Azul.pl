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
pieces_colors([blue,red,yellow,black,white]).
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
ejecute_round(_,[],_,[]).
ejecute_round(OldRound,[PlayerActual:Id|Players],NewRound,[Id:FacId|Accions]):-
    % Vemos cual es la estrategia del jugador actual
    member_dict(strategy,PlayerActual,St),
    % decimos quien juega ahora
    % info_log(["Player" ,Id,"empieza el turno"]),
    %  Se ejecuta la estrategia del jugador actual
    Play =..[St,OldRound,PlayerActual,ActualRound,NewPlayer,LineId:FacId:Color],
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
    assing(Id,OldBoard,NewPlayer,ActualBoard),
    assing(players,ActualRound,ActualBoard,NextRound),
    % manda a jugar al proximo jugador
    ejecute_round(NextRound,Players,NewRound,Accions).
% Se obtienes todas las posibles opciones de jugada y se toma la primera de la lista
basic(Round,Player,NewRound,NewPlayer,O):-
    options(Round,Player,[O|_]),
    !,
    update_player_board(Player,Round,O,NewPlayer,Return,_),
    update_game(Round,O,NewRound,Return).
% si la opcion no es valida , es decir no se puede poner en el muro de preparacion , de
% igual forma se toma la primera y penlaiza
basic(Round, Player, NewRound, NewPlayer, none:Id:Color) :-
    valid_colors(Round, [Amount:Id:Color | _]), !,
    update_game(Round, none:Id:Color, NewRound, Amount),
    Neg is Amount* -1,
    penalize(Player, Neg, NewPlayer).
basic(Round, Player, Round, Player, none:none:none).
% obtiene todos los movientos validos , para la zona de preparacion
options(Round , Player,Optionss):-
    member_dict(factories,Round,Fac),
    member_dict(board,Player,Board),
    findall(LineId:FacId:Color,(
        member_dict(LineId,Board,Line),
        member_dict(zone,Line,Zone),
        member(empty,Zone),
        member_dict(valid,Line,ValidColors),
        member_dict(FacId,Fac,ColFac),
        member(Color,ValidColors),
        member(Color,ColFac)
    ),  Optionss),
    not(length(Optionss,0)).
% Actualiza las lineas de la zona de preparacion, obtiene el tablero y verifiva si alguna de las filas 
% de preparacion se lleno y penaliza al jugador.
update_player_board(Player,Round,Line:Fac:Color,NewPlayer,Return,UpdatePlayer):-
    update_line(Player,Round,Line:Fac:Color,Player_Temp,Diff,Amount),
    member_dict(board,Player_Temp,Board),
    review_lines(Player_Temp,Board:sorted,UpdatePlayer),
    Return is Amount-Diff,
    penalize(Player_Temp,Diff,NewPlayer).
% Obtiene la zona de preparacion ,cuenta la cantidad de espacios vacios asi como la cantidad de piezas tomadas
% de la fabrica,luego colca las fichas tomadas en la zona de preparacion , todas las que sean posibles,se halla 
% la diferencia entre los espacios vacios y la cantidad de piezas tomadas
update_line(Player,Round,Line:F:Color,NewPlayer,Diff,Piece):-
    member_dict(fatories,Round,Factories),
    member_dict(F,Factories,Fac),
    member_dict(board,Player,Board),
    member_dict(L,Board,Line),
    member_dict(zone,Line,Zone),
    count(Zone,empty,Empty),
    count(Fac,Color,Amount),
    update(Zone,Amount,empty,Color,NewZone),
    count(NewZone,empty,NewEmpty),
    Diff is min(Empty-Amount,0),
    % saca las piezas del juego en caso de que sea necesario,luego actualiza los tablesros
    Piece is (min(NewEmpty - 1,0)* -(L-1)),
    assing(zone,Line,NewZone,NewLine),
    assing(valid,NewLine,[Color],ValidLine),
    assing(Line,Board,ValidLine,NewBoard),
    assing(board,Player,NewBoard,NewPlayer).
count(List,Value,R):-
    findall(1,member(Value,List),K),
    length(K,R).
% cambia en un numero igual a total de veces las repeticiones de 
% value en List y las sustituye por NewValue
update(List,0,_,_,List):-!.
update(List,_,Value,_,List):-
    not(member(Value,List)),!.
update(List,Total,Value,NewValue,R):-
    Total>0,
    Z is Total-1,
    my_concat(A,[Value|B],List),!,
    update(B,Z,Value,NewValue,K),
    my_concat(A,[NewValue|K],R).
% Mueve las piezas que no fueron tomadas a la fabricas al centro, y actualiza el estado del las fabricas
% que fueron vaciadas, actualiza la catidad de piezas que salen del juego en esat ronda
update_game(Round,_:Fac:Color,NewGame,ActualPiece):-
    member_dict(factorias,Round,FacRound),
    member_dict(Fac,FacRound,Factories),
    findall(X,(
        member(X,Factories),
        not(member(X,[empty,first,Color]))
    ),ToCenter),
    add([],4,empty,NewFac),
    assing(Fac,FacRound,NewFac,TempFacs),
    member_dict(center,TempFacs,Center),
    my_concat(ToCenter,Center,NewCenter),
    assing(center,TempFacs,NewCenter,NewFacs),
    assing(factorias,Round,NewFacs,Temp),
    member_dict(outs,Round,Outs),
    member_dict(Color,Outs,Number),
    Sum is Number + ActualPiece,
    assing(Color,Outs,Sum,NewOuts),
    assing(outs,Temp,NewOuts,NewGame).
% Obtiene todas las posibles formas de tomar las piezas de las fabricas
valid_colors(Round,Options):-
    member_dict(factories,Round,Fac),
    findall(Count:FacId,Color,(
        member_dict(FacId,Fac,F),
        member(Color,F),
        Color \= empty,
        count(F,Color,Count)
    ),  Options),
    not(length(Options,0)).
% Si el jugador toma mas piezas de las que puede poner en su zona de preparacion , entonces
% tiene que ser penalizado,luego se calcula en cuanto debe ser penalizado y se actualiza el 
% score del jugador , se repite el proceso hasta que sea penalizado un numero de veces igual 
% a la cantidad de fichas sin poner que tenia inicialemnte
penalize(Player,Amount,NewPlayer):-
    Amount<0,
    member_dict(penalties,Player,Penalties),
    length(Penalties,Sz),
    Sz >0,!,
    my_concat([P1],R,Penalties),
    assing(penalties,Player,R,TempPlayer1),
    member_dict(score,Player,Score),
    NewScore is Score+P1,
    assing(score,TempPlayer1,NewScore,TempPlayer2),
    Times is Amount+1,
    penalize(TempPlayer2,Times,NewPlayer).
penalize(Player,_,Player).
% reviza las lineas que estan llenas y las limpia
review_lines(P,[],P).
review_lines(Player,[_:Line|Lines],NewPlayer):-
    clean_line(Player,Line,ActualPlayer),!,
    review_lines(ActualPlayer,Line,NewPlayer).
review_lines(Player,[_|Lines],NewPlayer):-
    review_lines(Player,Lines,NewPlayer).
review_lines(Player,Lines:sorted,NewPlayer):-
    indexed_sort(Lines,Sorted),
    review_lines(Player,Sorted,NewPlayer).
% Obtiene la zona de preparacion del jugador y verfica si la linea que entra como parametro al 
% predicado esta completa , luego busca cual es el color que el que esta lleno esa linea y 
% actualiza los colores validos , luego se calcula la columa a la que le corresponde ese color
% en el muro,se actializa el score, y se actualiza todo el tablero del jugador
clean_line(Player,L,NewPlayer):-
    member_dict(board,Player,Board),
    member_dict(L,Board,Line),
    member_dict(all,Line,Colors),
    member_dict(valid,Line,[C]),
    member_dict(zone,Line,ActualZone),
    add([],L,C,ActualZone),
    my_concat(A,[C|B],Colors),
    my_concat(A,B,List),
    assing(all,Line,List,TempLine),
    assing(valid,TempLine,List,TempLine1),
    column_of(L,C,Column),
    update_score(Player,(L,Column),TempPlayer),
    add([],L,empty,Zone),
    assing(zone,TempLine1,Zone,TempLine2),
    assing(L,Board,TempLine2,NewBoard),
    assing(board,TempPlayer,NewBoard,NewPlayer).
% Se obtiene la lista de colores que coincie con los colores de la primera fila del muro
% se halla el idice del color y luego se calcula la columna en la que se encuentra es color 
% en linea que entra como parametro
column_of(Line,Color,Column):-
    pieces_colors(Colors),
    index_of(Color,Colors,Idx),
    Column is (Idx + Line-1)mod 5+1.
% se halla el index de el valor Value en list,para ello se halla la lista con la se concatena
% con Value y los elemtos que estan depues de el y se le halla el length
index_of(Value,List,Index):-
    my_concat(A,[Value|_],List),
    length(A,Index).
index_of(_,_,-1).
% Tomamos el board del jugador actual,revisamos si en la linea que entea como parametro se puede
% poner alguna pieza em caso de que se pueda poner se calculo el score de colocarla y se actualiza
% el score del jugador,en caso de que no se pueda poner la pieza , se queda igual es score del jugador
update_score(Player,(Line,Column),NewPlayer):-
    member_dict(board,Player,Board),
    member_dict(Line,Board,Lines),
    member_dict(zone,Lines,Zone),
    count(Zone,empty,0),!,
    pieces_score(Player,(Lines,Column),Score),
    member_dict(score,Player,PScore),
    Sum is Score+PScore,
    update_table(Player,(Line,Column),ActualPlayer),
    assing(score,ActualPlayer,Sum,NewPlayer).
update_score(P,_,P).
% Se obtiene el tablero del jugador actual,se le annade la nueva pieza,se halla ,puntuacion
% de la fila en la que se puso dicha pieza,se invierte el tablero y se vuelve a hallar, la 
% puntuacion de la fila en la que se puso la pieza ahora con el tablero invertido
% teniendo de esta manera la puntuacion por fila y por columna ,luego el score total 
% es la suma de ambos
pieces_score(Player,(Row,Column),Score):-
    member_dict(table,Player,Table),
    my_concat(Table,[(Row,Column)],NewTable),
    line_score(NewTable,(Row,Column),RowScore),
    invert_axis(NewTable,InvertedAxis),
    line_score(InvertedAxis,(Column,Row),RowScore),
    invert_axis(NewTable,InvertedAxis),
    line_score(InvertedAxis,(Column,Row),ColumnScore),
    Score is RowScore+ColumnScore.
% Busca los intervalos, en los que las piezas son adyacenytes,los recorre y busca 
% el itervalo al que pertenece la pieza que entra como parametro y devuleve el lenght
% de dicho intervalo
line_score(List,Piece,Score):-
    make_intervals(List,Interval),
    findall(X,(
        member(X,Interval),
        member(Piece,X)
    ), [Adyacents]),
    length(Adyacents,Score).
%Devuelve una lista con todos los elementos que sean adyacentes, si hay mas de uno no lo cuenta
make_intervals(List,Index):-
    is_list(List),
    sort(List,List_Sorted),
    blocks(List_Sorted,Index).
%Devuelve todos los elemetos que son consecutivos en la lista , si no devuelve una lista vacia
blocks([],[]).
blocks([X|L],Index):-
    consecutive(L,X,C,R),
    my_concat([X],C,B),
    blocks(R,K),
    my_concat([B],K,Index).
% Devuelve dos listas, la lista C donde van a estra los puntos que son adyacetes en L , y en R devuelve el resto
consecutive([(X,B)|L],(X,Y),C,R):-
    B is Y+1.!,
    consecutive(L,(X,B),A,R),
    my_concat([(X,B)],A,C).
consecutive(L,_,[],L).
% invierte L de forma tal que si en el existe el par X,Y , se obtiene en R el par Y,X
invert_axis(L,R):-
    findall((Y,X),member((X,Y),L),R).
% Toma el diccionario   que esat en L, lo invierte de forma tal que si existe X:Y en L entonces se obtiene
%  en R Y:X,lo ordena por Y y vuelve a invertir el diccionario como estaba inicialmente.
indexed_sort(L,R):-
    findall(X:Y,member_dict(X,L,Y),I),
    sort(I,O),
    findall(X:Y,member_dict(X,O,Y),R).
% Se obtienen , todas las posibles jugadas , se calcula que puntuacion se obtiene al efectuarlas,se guardan
% las jugados con su puntuacion se ordena en una lista , y nos quedamos la que nos de ayor score y esa es 
% finalmente la que realiza el jugador 
greedy(Round,Player,NewRound,NewPlayer,O):-
    options(Round,Player,Options),!,
    findall(Score:Options,(
        member(Options,Options),
        update_player_board(Player,Round,Options,_,_,Player_Temp),
        member_dict(score,Player_Temp,Score)
    ),Choice),
    sort(Choice,Sorted),
    my_concat(_,[_:O],Sorted),
    update_player_board(Player,Round,O,NewPlayer,Result,_),
    update_game(Round,O,NewRound,Result).
% Si no puede poner ninguna pieza en el tablero de preparcion , toma la opcion que menos lo penalize
greedy(Round,Player,NewRound,NewPlayer,Amount):-
    valid_colors(Round,Options),
    sort(Options,[Amount:id:Color|_]),
    update_game(Round,none:id:Color,NewRound,Amount),
    Neg is Amount* -1,
    penalize(Player,Neg,NewPlayer).
greedy(Round,Player,Round,Player,none:none:none).
% Annade la nueva pieza al tablero del jugador actual y le asigan el tablero actualizado al jugador actual
update_table(Player,Piece,NewPlayer):-
    member_dict(table,Player,Table),
    add(Table,1,Piece,NewTable),
    assing(table,Player,NewTable,NewPlayer).
%inicializa todas los componentes de los jugadores
clean_players(Round,NewRound):-
    member_dict(players,Round,Players),
    findall(Player:ID,(
        member(X:ID,Players),
        member_dict(board,X,Board),
        review_lines(X,Board:sorted,CleanedPlayer),
        create_ground(Penalizations),
        assing(penalization,CleanedPlayer, Penalizations,NewPlayer),
        member_dict(score,NewPlayer,ActualScore),
        Score is max(ActualScore,0),
        assing(score,NewPlayer,Score,Player)
    ), NewPlayers),
    assing(players,Round,NewPlayers,NewRound).
main(Player,Factories):-
    new_game(Players,Factories,Round),
    new_round(Round,NewRound),
    run(NewRound,[],EndedRound),!.
main(_,_).
new_game(Players,Factories,[P,A:amounts,O:out,F:factories]):-
    pieces_colors(C),
    create_players(Players,P),
    findall(20:X,member(X,C),A),
    findall(0:X,member(X,C),O),
    add([],4,empty,E),
    add([],Factories,E,EF),
    enumerate(EF,1,NF),
    assing(center,NF,[],F).
new_round(Round,NewRound):-
    fill(Round,TempRound1),
    member_dict(amounts,TempRound1,Amounts),
    findall(List,(
        member_dict(Color,Amounts,Count),
        add([],Count,Color,list)
    ),  ColorGroups),
    concat_all(ColorGroups,ColorList),
    random_permutation(ColorList,ColorOrder),
    member_dict(factories,TempRound1,RoundFac),
    remove_prop(center,RoundFac,SimpleFac),
    findall(Fac,member(Fac:_,SimpleFac),RawFac),
    use_fac(RawFac,ColorOrder,TempFac),
    concat_all(TempFac,UsedPieces),
    findall(NewQ:Color,(
        member_dict(Color,Amount,COld),
        count(UsedPieces,Color,Used),
        NewQ is COld-Used
    ) , NewAmounts),
    assing(amounts,TempRound1,NewAmounts,TempRound2),
    enumerate(TempFac,1,EnumFac),
    assing(center,EnumFac,[first],ALLFac),
    assing(factories,TempRound2,AllFac,NewRound).
fill(Round,NewRound):-
    member_dict(amounts,Round,Amount),
    member_dict(factories,Round,Factories),
    length(Factories,FacSz),
    findall(X,member(X:_,Amount),Count),
    sum_list(Count,Sum),
    Sum <FacSz*4,!,
    member_dict(ounts,Round,CAmount),
    findall(RealAmount:Color,(
        member_dict(color,Amounts,CAmount),
        member_dict(Color,Outs,COut),
        RealAmount is COut + CAmount
    ), NewAmounts),
    assing(amounts,Round,NewAmounts,TempRound),
    findall(0:Color,member(_:Color,Outs),Newouts),
    assing(outs,TempRound,NewOuts,NewRound).
fill(Round,Round).
concat_all([],[]).
concat_all([X|Y],R):-
    concat_all(Y,L),
    my_concat(X,L,R).
run(Round,Events,NewRound):-
    order_players(Round,Players),
    ejecute_round(Round,Players,TempRound,ActualEvents),
    my_concat(Events,ActualEvents,NewEvents),
    validate(TempRound,NewEvents,NewRound).
order_players(Round,NewPlayers):-
    member_dict(players,Round,Players),
    indexed_sort(Players,OriginalOrder),
    sort_players(OriginalOrder,NewPlayers).
sort_players(Players,NewPlayers):-
    initial_player(PId),
    my_concat(A,[Player:PId|B],Players),
    my_concat([Player:PId|B],A,NewPlayers).
initial_player(1).
validate(Round,Events,NewRound):-
    member_dict(factories,Round,Factories),
    findall(Fac,member(Fac:_,Factories),FacList),
    concat_all(FacList,AllPieces),
    length(AllPieces,Sz),
    count(AllPieces,empty,Sz),!,
    clean_players(Round,NewRound),
    final_or_continue(TempRound,Events,NewRound).
validate(Round,Events,NewRound):-
    run(Round,Events,NewRound).
final_or_continue(Round,_,NewRound):-
    ending_condition(Round),!,
    calculate_score(Round,NewRound).
final_or_continue(Round,Events,NewRound):-
    initial_player(ID),
    get_value(center,Events,NewID,ID),
    retract(initial_player(ID)),
    asserta(initial_player(NewID)),
    member_dict(players,Round,Players),
    member_dict(NewID,Players,FirstPlayer),   
    penalize(FirstPlayer,-1,NewFirstPlayer),
    assing(NewID,Players,NewFirstPlayer,NewPlayers),
    assing(players,Round,NewPlayers,TempRound1),
    new_round(TempRound1,TempRound2),
    run(TempRound2,[],NewRound).
get_value(P,O,V,_):-
    member_dict(P,O,V).
get_value(_,_,D,D).
ending_condition(Round):-
    member_dict(players,Round,P),
    member(X:_,P),
    any_full_row(X,_).
any_full_row(Player,RowC):-
    member_dict(table,Player,Table),
    findall(true,(
        bagof(Column, member((_,Column),Table),Column),
        length(Columns,5)
    ), Rows),
    length(Rows,RowsC),
    any(Rows).
any(true).
any(L):- 
    is_list(L),
    member(true,L).
calculate_score(Round,NewRound):-
    member_dict(ID,Players,PLayer),
    table_score(Player,TableScore),
    member_dict(score,Player,Score),
    NewScore is Score + TableScore,
    assing(players,Round,NewPlayers,NewRound).
table_score(P,S):-
    full_rows(P,RS),
    member_dict(table,P,T),
    invert_axis(T,RT),
    full_rows([RT:table],CS),
    full_colors(P,DS),
    S is RS*2+CS*7+10*Ds.
full_rows(Player,RowC):-
    any_full_row(Player,RowC),!.
full_rows(_,0).
full_colors(Player,Amount):-
    member_dict(table,Player,Table),
    findall(true,(
        member((1,Col),Table),
        cascade((1,Col),Table)
    ),  List),
    length(List,Amount).
cascade((5,Col),Table):-
    member((5,Col),Table).
cascade((Row,Column),Table):-
    member((Row,Column),Table),
    NewRow is Row + 1,
    NewCol is max((Col+1)mod 6,1),
    cascade((NewRow,NewCol),Table).
use_fac([],_,[]).
use_fac(Factories,[],Factories).
use_fac([[]|Factories],Pieces,[[]|Result]):-
    use_fac(Factories,Pieces,Result).
use_fac([[_|Fac1]|Factories],[Piece|Pieces],[[Piece|Res1]|Result]):-
    use_fac([Fac1|Factories],Pieces,[Res1|Result]).
remove_prop(_,[],[]).
remove_prop(P,[_:P|R],L):- !,
    remove_prop(P,R,L).
remove_prop(P,[X:Y|R],[X:Y|L]):-
    Y\=P,
remove_prop(P,R,L).

    