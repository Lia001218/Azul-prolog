palette_colors([blue, red, yellow, black, white]).

create_ground([-1, -1, -2, -2, -2, -3, -3]:penalties).

:- (dynamic initial_player/1).
initial_player(1).

main(Players, Factories) :-
    open("game_azul.txt", write, FD),
    write(FD,["Empieza el juego"]),
    nl(FD),
    close(FD),
    new_game(Players, Factories, Game),
    new_round(Game, NewGame),
    run(NewGame, [], EndedGame), !,
    open("game_azul.txt", append, Fd),
    write(Fd, "El juego a terminado y el ganador es ... \n"),
    close(Fd),
    printscore(EndedGame:scores).

main(_, _) :- 
    open("game_azul.txt", append, FD),
    write(FD,"An unexpected failure occur\n"),
    close(FD).
    % writeln("fail.").
% para crear un nuevo juego , primero creamos los jugadores , luego obtenemos las piezas que vamos a utilizar
% 20 de cada color , luego creamos una lista de tamanno 4 que de momento va a tener en todas las pocisiones empty
% luego creamos una lista donde cada posicion de la lista va ser la lista mencionada anteriormente, y ademas cada
% poscion va representar una fabrica
% NOTA IMPORTANTE !!!! Se agrega el centro vacio a la lista de fabricas, trataremos el centro como una fabrica , 
% lo que a diferecia del resto de las fabricas no va a estar enumerado , aparecera en la lista seguido de : center
new_game(Players, Factories, [P, A:ammounts, O:outs, F:factories]) :-
    palette_colors(C),
    create_new_players(Players, P),
    findall(20:X, member(X, C), A),
    findall(0:X, member(X, C), O),
    add([], 4, empty, E),
    add([], Factories, E, EF),
    enumerate(EF, 1, NF),
    set_dict(center, NF, [], F).
% Primero se comprueba cuantas piezas se necesitan para la siguiente ronda,luego toma todas las piezas
% que se tengan y seleccionan de forma random para empezarlas a annadir a las fabricas, se guardan las 
% piezas seleccionadas se actualizan la cantidad de pieazas y se crea una nueva ronda
new_round(Game, NewGame) :-
    open("game_azul.txt", append, FD),
    write(FD,"Starts New Round\n"),
    close(FD),
    fill(Game, TempGame1),
    find_dict(ammounts, TempGame1, Ammounts),
    findall(List, ( 
        find_dict(Color, Ammounts, Quantity),
        add([], Quantity, Color, List)
    ), ColorGroups),
    append_all(ColorGroups, ColorsList),
    random_permutation(ColorsList, ColorsOrder),
    find_dict(factories, TempGame1, GameFac),
    del_dict(center, GameFac, SimpleFac),
    findall(Fac, member(Fac:_, SimpleFac), RawFac),
    use_fac(RawFac, ColorsOrder, TempFac),
    append_all(TempFac, UsedTiles),
    findall(NewQ:Color, ( 
        find_dict(Color, Ammounts, QOld),
        count(UsedTiles, Color, Used),
        NewQ is QOld-Used
    ), NewAmmounts),
    set_dict(ammounts, TempGame1, NewAmmounts, TempGame2),
    enumerate(TempFac, 1, EnumFac),
    set_dict(center, EnumFac, [first], AllFac),
    set_dict(factories, TempGame2, AllFac, NewGame).
% Ejcuta el juego hasta el final , obtiene las acciones de los jugadores en la ronda actual, ejcuta las instrucciones por jugador
% comprueba si la ronda actual es el fin del juego si lo es finaliza y en caso contrario continua 
% ejcutando
run(Game, Events, NewGame) :-
    new_order(Game, Players),
    run_round(Game, Players, TempGame, CurEvents),
    append(Events, CurEvents, NewEvents),
    validate(TempGame, NewEvents, NewGame).
% se crea una lista de tamanno igual a la cantidad de jugadores,y se le annade a cada posicion (que cada posicion
% representa un jugador) la zona de preparacion y el piso,luego se le asigana una estrategia de forma ramdon a cada
% jugador y se crea una lista en la cual en la pocision i contando a partir de 1 va a estar la estrategia del jugador
% i,zona de preparacion del jugador i,piso del jugaor i, muro del jugador i y score del jugador i, de forma tal que 
% la posicion i de la lista tiene toda la informacion del jugador i
create_new_players(Ammount, Players:players) :-
    create_board(Board),
    create_ground(Penalties),
    add([], Ammount, [Board, Penalties, []:wall, 0:score], List),
    findall(P, (
        member(X, List),
        random_strategy(S),
        set_dict(strategy, X, S, P)
    ), RawPlayers),
    enumerate(RawPlayers, 1, Players).
% se encarga de que las fabricas esten llenas en la siguiente ronda
fill(Game, NewGame) :-
    find_dict(ammounts, Game, Ammounts),
    find_dict(factories, Game, Factories),
    length(Factories, FacSz),
    findall(X, member(X:_, Ammounts), Quantities),
    sum_list(Quantities, Sum),
    Sum<FacSz*4, !,
    find_dict(outs, Game, Outs),
    open("game_azul.txt", append, FD),
    write(FD,["Añadiendo piezas a la bolsa.\n\t", Ammounts:ammounts, "\n\t", Outs:outs]),
    nl(FD),
    close(FD),
    findall(RealAmmount:Color, (
        find_dict(Color, Ammounts, QAmmount),
        find_dict(Color, Outs, QOut),
        RealAmmount is QOut+QAmmount
    ), NewAmmounts),
    set_dict(ammounts, Game, NewAmmounts, TempGame),
    findall(0:Color, member(_:Color, Outs), NewOuts),
    set_dict(outs, TempGame, NewOuts, NewGame).
fill(Game, Game).
% Se encarga de llenar las fabricas 
use_fac([], _, []).
use_fac(Factories, [], Factories).
use_fac([[]|Factories], Tiles, [[]|Result]) :-
    use_fac(Factories, Tiles, Result).
use_fac([[_|Fac1]|Factories], [Tile1|Tiles], [[Tile1|Res1]|Result]) :-
    use_fac([Fac1|Factories], Tiles, [Res1|Result]).
% Rotorna el orden de los jugadores en el ronda actual
new_order(Game, NewPlayers) :-
    find_dict(players, Game, Players),
    indexed_sort(Players, OriginalOrder),
    sort_players(OriginalOrder, NewPlayers).

run_round(G, [], G, []).
run_round(Game, [P1:Id|Players], NewGame, [Id:Fid|Events]) :-
    find_dict(strategy, P1, St),
    open("game_azul.txt", append, Fd),
    write(Fd,["Player ", Id, " with Strategy ",St, " turn start --------------------"]),
    nl(Fd),
    close(Fd),
    Move=..[St, Game, P1, TempGame1, NewP1, Lid:Fid:Color],
    Move,
    open("game_azul.txt", append, FD),
    write(FD, [
        "Player choose all type ",
        Color,
        " from expositor ",
        Fid,
        " and add them to the ",
        Lid,
        " line"
    ]),
    nl(FD),
    close(FD),
    find_dict(factories, TempGame1, _),
    print_preparation(NewP1:preparation),

    find_dict(players, TempGame1, OldPlayers),
    set_dict(Id, OldPlayers, NewP1, CurPlayers),
    set_dict(players, TempGame1, CurPlayers, TempGame2),

    print_wall(NewP1:wall),
    run_round(TempGame2, Players, NewGame, Events).

% comprueba si en esta ronda se acaba el juego 
validate(Game, Events, NewGame) :-
    find_dict(factories, Game, Factories),
    findall(Fac, member(Fac:_, Factories), FacList),
    append_all(FacList, AllTiles),
    length(AllTiles, Sz),
    count(AllTiles, empty, Sz), !,
    refresh_players(Game, TempGame),
    end_continue(TempGame, Events, NewGame).
validate(Game, Events, NewGame) :-
    run(Game, Events, NewGame).
% primero se crea una lista de 5 elementos , luego se enumera de izquierda a derecha la lista , de forma tal que 
% en valor que le asigne el enum va a ser la cantidad de columnas que va a tener esa fila en la zona de preparacion
% una vez hecho esto teniendo en cuenta le numero que le asigan el enum a la posicion i , se utiliza el metoda add
% que crea una nueva lista a la cual le annade empty la cantidad de veces que diga el enum y con el metodo findall
% a su vez va creando una lista con dichas listas y ademas tambien guarda en cada pocicion i los colores que se 
% pueden poner en la lista i (en principio todos) y todos los colores posibles del juego
create_board(Data:board) :-
    add([], 5, 1, List),
    enumerate(List, 1, Enum),
    palette_colors(C),
    findall([New:prep, C:valid, C:all]:Sz, (
        find_dict(Sz, Enum, _),
        add([], Sz, empty, New)
    ), Data).

sort_players(Players, NewPlayers) :-
    initial_player(Pid),
    append(A, [Player:Pid|B], Players),
    append([Player:Pid|B], A, NewPlayers).
% si la opcion no es valida , es decir no se puede poner en el muro de preparacion , de
% igual forma se toma la primera y penlaiza
basic(Game, Player, NewGame, NewPlayer, A) :-
    option_to_play(Game, Player, [A|_]), !,
    update_player(Player, Game, A, NewPlayer, Return, _, 1),
    update_game(Game, A, NewGame, Return).
basic(Game, Player, NewGame, NewPlayer, none:Id:Color) :-
    valid_colors(Game, [Ammount:Id:Color | _]), !,
    update_game(Game, none:Id:Color, NewGame, Ammount),
    Neg is Ammount* -1,
    penalize(Player, Neg, NewPlayer,1).
basic(Game, Player, Game, Player, none:none:none).
% Escoge una opcion de forma random
random(Game, Player, NewGame, NewPlayer, A) :-
    option_to_play(Game, Player, Moves), !,
    random_permutation(Moves, [A|_]),
    update_player(Player, Game, A, NewPlayer, Return, _, 1),
    update_game(Game, A, NewGame, Return).
random(Game, Player, NewGame, NewPlayer, none:Id:Color) :-
    valid_colors(Game, [Ammount:Id:Color | _]), !,
    update_game(Game, none:Id:Color, NewGame, Ammount),
    Neg is Ammount* -1,
    penalize(Player, Neg, NewPlayer,1).
random(Game, Player, Game, Player, none:none:none).

% Se obtienen , todas las posibles jugadas , se calcula que puntuacion se obtiene al efectuarlas,se guardan
% las jugados con su puntuacion se ordena en una lista , y nos quedamos la que nos de ayor score y esa es 
% finalmente la que realiza el jugador 
greedy(Game, Player, NewGame, NewPlayer, A) :-
    option_to_play(Game, Player, Moves), !,
    findall(Score:Move, (
        member(Move, Moves),
        update_player(Player, Game, Move, _, _, TempPlayer, 0),
        find_dict(score, TempPlayer, Score)
    ), Options),
    sort(Options, Sorted),
    append(_, [_:A], Sorted),
    update_player(Player, Game, A, NewPlayer, Return, _, 1),
    update_game(Game, A, NewGame, Return).
greedy(Game, Player, NewGame, NewPlayer, none:Id:Color) :-
    valid_colors(Game, Moves), !,
    sort(Moves, [Ammount:Id:Color|_]),
    update_game(Game, none:Id:Color, NewGame, Ammount),
    Neg is Ammount* -1,
    penalize(Player, Neg, NewPlayer,1).
greedy(Game, Player, Game, Player, none:none:none).

refresh_players(Game, NewGame) :-
    find_dict(players, Game, Players),
    findall(Player:Id, (
        member(X:Id, Players),
        find_dict(board, X, Board),
        verify_line(X, Board:unsorted, CleanedPlayer),
        create_ground(Penalizations),
        set_dict(penalization, CleanedPlayer, Penalizations, NewPlayer),
        find_dict(score, NewPlayer, CurScore),
        Score is max(CurScore, 0),
        set_dict(score, NewPlayer, Score, Player)
    ), NewPlayers),
    set_dict(players, Game, NewPlayers, NewGame).
%  Verifica si el juego a terminado de ser asi calcula la putuacion y en caso contrario sigue ejucutando
% el juego
end_continue(Game, _, NewGame) :-
    ending_condition(Game), !,
    calculate_scores(Game, NewGame).
end_continue(Game, Events, NewGame) :-
    initial_player(Id),
    find_default(center, Events, NewId, Id),
    retract(initial_player(Id)),
    asserta(initial_player(NewId)),
    find_dict(players, Game, Players),
    find_dict(NewId, Players, FirstPlayer),
    penalize(FirstPlayer, -1, NewFirstP,1),
    open("game_azul.txt", append, FD),
    write(FD,["Player ", NewId, " gets penalized for being the first to play next turn"]),
    nl(FD),
    close(FD),
    set_dict(NewId, Players, NewFirstP, NewPlayers),
    set_dict(players, Game, NewPlayers, TempGame1),
    new_round(TempGame1, TempGame2),
    run(TempGame2, [], NewGame).
% reviza las lineas que estan llenas y las limpia
verify_line(P, [], P).
verify_line(Player, [_:Line|Lines], NewPlayer) :-
    clean_line(Player, Line, CurPlayer), !,
    verify_line(CurPlayer, Lines, NewPlayer).
verify_line(Player, [_|Lines], NewPlayer) :-
    verify_line(Player, Lines, NewPlayer).
verify_line(Player, Lines:unsorted, NewPlayer) :-
    indexed_sort(Lines, Sorted),
    verify_line(Player, Sorted, NewPlayer).
%  calcula la puntuacion de los jugadores segun las relas del juego
calculate_scores(Game, NewGame) :-
    find_dict(players, Game, Players),
    findall(NewPlayer:Id, ( 
        find_dict(Id, Players, Player),
        wall_score(Player, WallScore),
        find_dict(score, Player, Score),
        NewScore is Score+WallScore,
        set_dict(score, Player, NewScore, NewPlayer)
    ), NewPlayers),
    set_dict(players, Game, NewPlayers, NewGame).

ending_condition(Game) :-
    find_dict(players, Game, P),
    member(X:_, P),
    any_full_row(X, _).
% Si el jugador toma mas piezas de las que puede poner en su zona de preparacion , entonces
% tiene que ser penalizado,luego se calcula en cuanto debe ser penalizado y se actualiza el 
% score del jugador , se repite el proceso hasta que sea penalizado un numero de veces igual 
% a la cantidad de fichas sin poner que tenia inicialemnte
penalize(Player, Ammount, NewPlayer, Test) :-
    Ammount<0,
    find_dict(penalties, Player, Penalties),
    length(Penalties, Sz),
    Sz>0, !,
    append([P1], R, Penalties),

    printif(Test,["Player recive ", P1, " of penalization"]),
    set_dict(penalties, Player, R, TempPlayer1),
    find_dict(score, Player, Score),
    NewScore is Score+P1,
    set_dict(score, TempPlayer1, NewScore, TempPlayer2),
    Times is Ammount+1,
    penalize(TempPlayer2, Times, NewPlayer, Test).
penalize(Player, _, Player, _).
% Obtiene la zona de preparacion del jugador y verfica si la linea que entra como parametro al 
% predicado esta completa , luego busca cual es el color que el que esta lleno esa linea y 
% actualiza los colores validos , luego se calcula la columa a la que le corresponde ese color
% en el muro,se actializa el score, y se actualiza todo el tablero del jugador
clean_line(Player, L, NewPlayer) :-
    find_dict(board, Player, Board),
    find_dict(L, Board, Line),
    find_dict(all, Line, Colors),
    find_dict(valid, Line, [C]),
    find_dict(prep, Line, CurPrep),

    add([], L, C, CurPrep),
    append(A, [C | B], Colors),
    append(A, B, List),
    set_dict(all, Line, List, TempLine0),
    set_dict(valid, TempLine0, List, TempLine1),
    % update the player
    find_column(L, C, Column),
    update_score(Player, (L, Column), TempPlayer),
    % cleaning the line
    add([], L, empty, Prep),
    set_dict(prep, TempLine1, Prep, TempLine2),
    set_dict(L, Board, TempLine2, NewBoard),
    set_dict(board, TempPlayer, NewBoard, NewPlayer).


wall_score(P, S) :-
    full_rows(P, RS),
    find_dict(wall, P, T),
    inver_matrix(T, RT),
    full_rows([RT:wall], CS),
    full_colors(P, DS),
    S is RS*2+CS*7+10*DS.
% Tomamos el board del jugador actual,revisamos si en la linea que entea como parametro se puede
% poner alguna pieza em caso de que se pueda poner se calculo el score de colocarla y se actualiza
% el score del jugador,en caso de que no se pueda poner la pieza , se queda igual es score del jugador
update_score(Player,  (L, C), NewPlayer) :-
    find_dict(board, Player, Board),
    find_dict(L, Board, Line),
    find_dict(prep, Line, Prep),
    count(Prep, empty, 0), !,
    pieces_score(Player,  (L, C), Score),
    find_dict(score, Player, PScore),
    Sum is Score+PScore,
    update_wall(Player,  (L, C), CurPlayer),
    set_dict(score, CurPlayer, Sum, NewPlayer).
update_score(P, _, P).
% Se obtiene el tablero del jugador actual,se le annade la nueva pieza,se halla ,puntuacion
% de la fila en la que se puso dicha pieza,se invierte el tablero y se vuelve a hallar, la 
% puntuacion de la fila en la que se puso la pieza ahora con el tablero invertido
% teniendo de esta manera la puntuacion por fila y por columna ,luego el score total 
% es la suma de ambos
pieces_score(Player,  (Row, Column), Score) :-
    find_dict(wall, Player, Wall),
    append(Wall, [(Row, Column)], NewWall),
    % row score
    line_score(NewWall,  (Row, Column), RowScore),
    inver_matrix(NewWall, InvertedAxis),
    % column score
    line_score(InvertedAxis,  (Column, Row), ColumnScore),
    Score is RowScore+ColumnScore.

update_wall(Player, Tile, NewPlayer) :-

    find_dict(wall, Player, Wall),
    add(Wall, 1, Tile, NewWall),
    set_dict(wall, Player, NewWall, NewPlayer).
% Busca los intervalos, en los que las piezas son adyacenytes,los recorre y busca 
% el itervalo al que pertenece la pieza que entra como parametro y devuleve el lenght
% de dicho intervalo
line_score(List, Tile, Score) :-
    get_all_adjacent(List, Interval),
    findall(X, ( 
        member(X, Interval),
        member(Tile, X)
    ), [Adyacents]),
    length(Adyacents, Score).
% obtiene todos los movientos validos , para la zona de preparacion
option_to_play(Game, Player, Moves) :-
    find_dict(factories, Game, Fac),
    find_dict(board, Player, Board),
    findall(Lid:Fid:Color,( 
        find_dict(Lid, Board, Line),
        find_dict(prep, Line, Prep),
        member(empty, Prep),
        find_dict(valid, Line, ValidColors),
        find_dict(Fid, Fac, CurFac),
        member(Color, ValidColors),
        member(Color, CurFac)
    ), Moves),
    not(length(Moves, 0)).    
% Actualiza las lineas de la zona de preparacion, obtiene el tablero y verifiva si alguna de las filas 
% de preparacion se lleno y penaliza al jugador.
update_player(Player, Game, L:F:Color, NewPlayer, Return, FinalPlayer, Test) :-
    update_line(Player, Game, L:F:Color, TempPlayer0, Diff, Ammount, Test),
    find_dict(board, TempPlayer0, Board),
    verify_line(TempPlayer0, Board:unsorted, FinalPlayer),
    Return is Ammount-Diff,
    penalize(TempPlayer0, Diff, NewPlayer,Test).

% Mueve las piezas que no fueron tomadas a la fabricas al centro, y actualiza el estado del las fabricas
% que fueron vaciadas, actualiza la catidad de piezas que salen del juego en esat ronda
update_game(Game, _:F:C, NewGame, ReturnedTiles) :-

    find_dict(factories, Game, GameFac),
    find_dict(F, GameFac, Fac),
    findall(X, (
        member(X, Fac),
        not(member(X, [empty, first, C]))
    ), ToCenter),
    add([], 4, empty, NewFac),
    set_dict(F, GameFac, NewFac, TempFacs),
    find_dict(center, TempFacs, Center),
    append(ToCenter, Center, NewCenter),
    set_dict(center, TempFacs, NewCenter, NewFacs),
    % writeln(["New factories center is -> ", NewCenter]),
    set_dict(factories, Game, NewFacs, Temp),
    find_dict(outs, Game, Outs),
    find_dict(C, Outs, Number),
    Sum is Number+ReturnedTiles,
    set_dict(C, Outs, Sum, NewOuts),
    set_dict(outs, Temp, NewOuts, NewGame).
% Obtiene la zona de preparacion ,cuenta la cantidad de espacios vacios asi como la cantidad de piezas tomadas
% de la fabrica,luego colca las fichas tomadas en la zona de preparacion , todas las que sean posibles,se halla 
% la diferencia entre los espacios vacios y la cantidad de piezas tomadas
update_line(Player, Game, L:F:Color, NewPlayer, Diff, Tiles, Test) :-
    find_dict(factories, Game, Factories),
    find_dict(F, Factories, Fac),
    find_dict(board, Player, Board),
    find_dict(L, Board, Line),
    find_dict(prep, Line, Prep),
    count(Prep, empty, Empty),
    count(Fac, Color, Ammount),
    replace(Prep, Ammount, empty, Color, NewPrep),
    count(NewPrep, empty, NewEmpty),
    Diff is min(Empty-Ammount, 0),
    Tiles is (min(NewEmpty - 1, 0) * -(L - 1)),

    printif(Test, ["Player modified Line ", L, " -> ", NewPrep]),

    set_dict(prep, Line, NewPrep, NewLine),
    set_dict(valid, NewLine, [Color], ValidLine),
    set_dict(L, Board, ValidLine, NewBoard),
    set_dict(board, Player, NewBoard, NewPlayer).


valid_colors(Game, Moves) :-
    find_dict(factories, Game, Fac),
    findall(Count:Fid:Color, ( 
        find_dict(Fid, Fac, F),
        member(Color, F),
        Color \= empty,
        count(F, Color, Count)
    ), Moves),
    not(length(Moves, 0)).  

printif(1, Text):-
    open('game_azul.txt', append, Stream),
    write(Stream, Text),
    nl(Stream),
    close(Stream).
printif(_, _).
%  annade K veces el elemento X a L
add(L, 0, _, L).
add(L, K, X, R) :-
    K>0,
    P is K-1,
    append(L, [X], L1),
    add(L1, P, X, R).


isList([]).
isList([_|_]).


any(true).
any(L) :-
    isList(L),
    member(true, L).


get_adj([(X, B)|L],  (X, Y), C, R) :-
    B is Y+1, !,
    get_adj(L,  (X, B), A, R),
    append([(X, B)], A, C).
get_adj(L, _, [], L).


adjacent([], []).
adjacent([X|L], I) :-
    get_adj(L, X, C, R),
    append([X], C, B),
    adjacent(R, K),
    append([B], K, I).


get_all_adjacent(L, I) :-
    isList(L),
    sort(L, S),
    adjacent(S, I).


find_index(V, L, I) :-
    append(A, [V|_], L),
    length(A, I).
find_index(_, _, -1).


find_dict(P, O, V) :-
    member(V:P, O).


find_default(P, O, V, _) :-
    find_dict(P, O, V).
find_default(_, _, D, D).


del_dict(_, [], []).
del_dict(P, [_:P|R], L) :- !,
    del_dict(P, R, L). 
del_dict(P, [X:Y|R], [X:Y|L]) :-
    Y\=P,
    del_dict(P, R, L).


set_dict(P, O, V, N) :-
    del_dict(P, O, C),
    append([V:P], C, N).


replace(L, 0, _, _, L) :- !.
replace(L, _, V, _, L) :-
    not(member(V, L)), !.
replace(L, T, V, N, R) :-
    T>0,
    Z is T-1,
    append(A, [V|B], L), !,
    replace(B, Z, V, N, K),
    append(A, [N|K], R).


append_all([], []).
append_all([X|Y], R) :-
    append_all(Y, L),
    append(X, L, R).


find_column(Line, Color, Column) :-
    palette_colors(Colors),
    find_index(Color, Colors, Idx),
    Column is ((Idx+Line-1)mod 5 ) +1.


count(L, V, R) :-
    findall(1, member(V, L), K),
    length(K, R).

enumerate([], _, []).
enumerate([E1|List], Number, [E1:Number|Enum]) :-
    Next is Number+1,
    enumerate(List, Next, Enum).


inver_matrix(L, R) :-
    findall((Y, X), member((X, Y), L), R).


indexed_sort(L, R) :-
    
    findall(X:Y, find_dict(X, L, Y), I),
    sort(I, O),
    findall(X:Y, find_dict(X, O, Y), R).
% comprueba si el jugador tiene alguna fila completa en su pared
any_full_row(Player, RowsQ) :-
    find_dict(wall, Player, Wall),
    findall(true, ( 
        bagof(Column, member((_, Column), Wall), Columns),
        length(Columns, 5)
    ), Rows),
    length(Rows, RowsQ),
    any(Rows).

full_rows(Player, RowsQ) :-
    any_full_row(Player, RowsQ), !.
full_rows(_, 0).

cascade((5, Col), Wall) :-
    member((5, Col), Wall).
cascade((Row, Col), Wall) :-
    member((Row, Col), Wall),
    NewRow is Row+1,
    NewCol is max((Col+1)mod 6, 1),
    cascade((NewRow, NewCol), Wall).

full_colors(Player, Ammount) :-
    find_dict(wall, Player, Wall),
    findall(true, ( 
        member((1, Col), Wall),
        cascade((1, Col), Wall)
    ), List),
    length(List, Ammount).    
printscore(EndedGame:scores) :-

    open("game_azul.txt", append, FD),
    find_dict(players, EndedGame, Players),
    nl(FD),
    findall([Id:id, Strategy:strategy]:Score, ( 
        member(X:Id, Players),
        find_dict(score, X, Score),
        find_dict(strategy, X, Strategy)
    ), PlayersInverted),
    indexed_sort(PlayersInverted, PlayersSorted),
    reverse(PlayersSorted, P),
    findall(P, (
        member([Id:_|_]:Score, P),
        write(FD,"Player "),
        write(FD, Id),
        write(FD," => "),
        write(FD,Score),
        nl(FD)
        ),_),
    close(FD).


select_strategy([basic, greedy, random]).

random_strategy(S) :-
    select_strategy(St),
    random_permutation(St, [S|_]).

print_preparation(Player:preparation) :-
    open("game_azul.txt", append, Fd),
    write(Fd, "Preparation Zone of Player: \n"),
    nl(Fd),
    find_dict(board, Player, Board),
    findall(Line:Id, (
        member(X:Id, Board),
        find_dict(prep, X, Prep),
        Times is 5 - Id,
        add(Prep, Times, ' - ', Line)
        ), Lines),
    indexed_sort(Lines, Sorted),
    findall(Line, (
            member(X:_, Sorted),
            write(Fd, X),
            nl(Fd)
        ), _) ,
    nl(Fd),
    close(Fd).

create_wall((6,1), _, Wall, Wall).
create_wall((Row,6), Board, Ac, [Ac|R]) :-
    NewRow is Row+1,
    create_wall((NewRow,1),Board, [], R),!.
create_wall((X,Y), Table, Ac, R) :-
    member((X,Y), Table), !,
    find_column(X,C,Y),
    append(Ac, [C], NewAc),
    NewY is Y+1,
    create_wall((X,NewY), Table, NewAc, R).
create_wall((X,Y), Table, Ac, R) :-
    append(Ac, [' - '], NewAc),
    NewY is Y+1,
    create_wall((X,NewY), Table, NewAc, R).

print_wall(Player:wall) :-
    open("game_azul.txt", append, Fd),
    write(Fd, "Wall of Player: \n"),
    nl(Fd),
    find_dict(wall, Player, Wall),
    sort(Wall, SortedWall),
    create_wall((1,1), SortedWall ,[], FinalWall),
    findall(FinalWall, (
        member(X, FinalWall),
        write(Fd,X),
        nl(Fd)
    ), _),
    nl(Fd),
    close(Fd).
