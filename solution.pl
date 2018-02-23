% We begin our Great Adventure !

/* build_error_message(+Type, +Var, -Message)
 * Constructs an error message of the given @Type
 * With the possible help of the information @Var
 * And put it inside the @Message
 */
build_error_message(Type, Var, Message) :-
    % The table does not exist
    Type = no_table, !,
    string_concat("Table ", Var, Temp),
    string_concat(Temp, " does not exist.", Message);

    % The table already exist
    Type = already_exists, !,
    string_concat("Table ", Var, Temp),
    string_concat(Temp, " already exists in database.", Message);

    % The data is not a list
    Type = not_a_list, !,
    Message = "Not a valid input";

    % Bad row insertion
    Type = missing_columns, !,
    (Found, Existing) = Var,
    string_concat("Row conflict. You tried to insert ", Found, Temp),
    string_concat(Temp, " rows while the table has ", Temp2),
    string_concat(Temp2, Existing, Temp3),
    string_concat(Temp3, " row(s).", Message);

    % The exception is unknown
    throw("Incexception : unexpected exception").

/* print_elems(+List)
 * Prints the elements of the @List,
 * one element per line
 */
print_elems([]).
print_elems([Head|Tail]) :-
    writeln(Head),
    print_elems(Tail).

/* assert_table_exists(+Table)
 * True if @Table exists, False otherwise
 */
assert_table_exists(Table) :-
    tables(Tables),
    member(Table, Tables).

/* index_of(+Columns, +Column, -Index)
 * Get the @Index of the @Column inside @Columns
 */
index_of([Head|_], Column, 0) :- !.
index_of([_|Tail], Column, Index) :-
  index_of(Tail, Column, SubIndex),
  !,
  SubIndex is Index+1.

replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- dif(H,O), replace(O, R, T, T2).

tables :-
    findall(X, table(X,_,_), Tables), % find all tables
    print_elems(Tables).               % display them

tables(Tables) :-
    findall(X, table(X,_,_), Tables).

assert_full_col_name(Table, Col, VerifiedCol) :-
  (
    X/Y = Col
    ->
      VerifiedCol = Col
    ;
      VerifiedCol = Table/Col
  ).

create(Table, Cols) :-
    % Todo : check if two cols do not have the same name
    % Todo : check that Cols is not empty
    tables(Tables),
    (
    member(Table, Tables)
        ->
        !,
        build_error_message(already_exists, Table, Message),
        throw(Message)
        ;
        length(Cols, Length),
        assert(table(Table, Cols, Length))
    ).

cols(Table, Cols) :-
    tables(Tables),
    (
    member(Table, Tables)
        ->
        table(Table, Cols, _)
        ;
        !,
        build_error_message(no_table, Table, Message),
        throw(Message)
    ).

rows(Table) :-
    % Todo : error message if table empty
    tables(Tables),
    (
    member(Table, Tables)
        ->
        findall(X, row(Table, X), Rows),
        print_elems(Rows)
        ;
        !,
        build_error_message(no_table, Table, Message),
        throw(Message)
    ).

insert(Table, Row) :-
    % Check that we want to insert a list
    not(is_list(Row)),
    build_error_message(not_a_list, _, Message),
    !, throw(Message);

    % If table exists
    assert_table_exists(Table), !,
    table(Table, _, Length),
    length(Row, Found),
    (Length = Found
        ->
            assert(row(Table, Row))
        ;
        build_error_message(
            missing_columns,
            (Length, Found),
            Message),
        !, throw(Message)
    )
    ;

    % Table does not exist
    build_error_message(no_table, Table, Message),
    throw(Message).

drop(Table) :-
    (
        member(Table, Tables)
        ->
        retractall(row(Table,_)),
        retract(table(Table,_,_))
        ;
        build_error_message(no_table,Table,Message),
        !,throw(Message)
    ).

delete(Table) :-
    (
        member(Table,Tables)
        ->
        retractall(row(Table,_))
        ;
        build_error_message(no_table,Table,Message),
        !,throw(Message)
    ).

delete(Table,Conds) :-
    (
        member(Table,Tables)
        ->
        handle_conds(Conds,ListofArgs),
        retract(row(Table,ListofArgs))
        ;
        build_error_message(no_table,Table,Message),
        !,throw(Message)
    ).

handle_conds([],ListofArgs).
handle_conds([H|T],ListofArgs) :-
    arg(2,H,V),
    handle_conds(T,[ListofArgs|V]).

replace_plus(TableName, ToRemove, Removed) :-

  (ToRemove = +X
  ->
  Removed = TableName/X
  ;
  ToRemove = Removed
  ).

table_columns(Table, Columns) :-
  table(Table, Columns,_).

/* Old version not accomodating un-tabled column names
parse_selectors(TableOrTables, Selectors, ParsedSelectors) :-
  (
    TableOrTables = [Htable|Ttable]
    ->
      (
        Selectors = [Hsel,Tpoivre]
        ->
          maplist(replace_plus(Htable), Selectors, ParsedSelectors)
        ;
          (
            Selectors = +X
            ->
              ParsedSelectors = HTable/X
            ;
              maplist(table_columns, TableOrTables, TempSelectors),
              flatten(TempSelectors, ParsedSelectors)
          )
      )
    ;
      (
        is_list(Selectors)
        ->
          maplist(replace_plus(TableOrTables), Selectors, ParsedSelectors)
        ;
          table_columns(TableOrTables, ParsedSelectors)
      )

  ).*/
parse_selectors([],Selectors, ParsedSelectors) :-
  !,flatten(Selectors, ParsedSelectors).
parse_selectors([Htable|Ttable], [], ParsedSelectors).
parse_selectors([Htable|Ttable], Selectors, ParsedSelectors) :-
    Selectors = +X,
    ParsedSelectors = HTable/X.

parse_selectors([Htable|Ttable], [HSelector|TSelector], ParsedSelectors) :-
  maplist(replace_plus(Htable), [HSelector,TSelector], RawParsedSelectors),
  !,flatten(RawParsedSelectors,ParsedSelectors).
parse_selectors(Table, Selectors, ParsedSelectors) :-
  (
    is_list(Selectors)
    ->
      maplist(replace_plus(Table), Selectors, ParsedSelectors)
    ;
      table_columns(Table, RawParsedSelectors),
      maplist(assert_full_col_name(Table), RawParsedSelectors, ParsedSelectors)
  ).


selec(TableOrTables, Selectors, Conds, Projection) :-
  parse_selectors(TableOrTables, Selectors, ParsedSelectors),
  %Conds = [H|T],
  %!, selec_one(TableOrTables, Selectors, H, Projection);
  !, selec_one(TableOrTables, ParsedSelectors, Conds, Projection).
  %!, selec_whole_table(TableOrTables, Projection). %Todo Remove

%Currently Working for only one table and condition, selector still useless
selec_one(Table, Selectors, Conds, Projection) :-
  (
    Table = [H|T]
    ->
      maplist(table_columns, Table, TempColumns),
      flatten(TempColumns,Columns),
      maplist(row, [persons,cities],TempRow),
      flatten(TempRow, Row)
    ;
      table(Table, RawColumns,_),
      maplist(assert_full_col_name(Table),RawColumns,Columns),
      row(Table, Row)
  ),
  %row(Table, Row), %Get row

  condition_loop(Conds, Columns, Row),
  (is_list(Selectors)
  ->
    selector(Columns, Selectors, Row, [], Selection),
    Projection = Selectors/Selection
  ;
    Projection = Selectors/Row
  ).

condition_loop([], _, _).
condition_loop([H|T], Columns, Row) :-
  (First, Remain) = H,
  condition_loop([First,Remain], Columns, Row);

  H=..CondList, %Transform the condition into a standard representation
  replace(+X,Table/X,CondList,OrderList), %Convert the '+' notation to the standard table/column
  OrderList = [Operator, Column, ExpectedValue],
  nth0(ColumnIndex, Columns, Column),
  nth0(ColumnIndex, Row, FoundValue),
  (_/_ = ExpectedValue
    ->
      nth0(SecondColumnIndex, Columns, ExpectedValue),
      nth0(SecondColumnIndex, Row, SecondFoundValue),
      Condition=..[Operator,FoundValue,SecondFoundValue]
    ;
      Condition=..[Operator,FoundValue,ExpectedValue]
  ),
  !,Condition,
  condition_loop(T, Columns, Row).

selector([],_,_,Builder, Projection) :-
  Builder = Projection.
selector([CurCol|RemCol], ColumnsKept, [CurRow|RemRow], Builder, Projection) :-
  (
    member(CurCol, ColumnsKept)
    ->
      append(Builder, [CurRow], ExpandedBuilder),
      selector(RemCol, ColumnsKept, RemRow, ExpandedBuilder, Projection)
    ;
      selector(RemCol, ColumnsKept, RemRow, Builder, Projection)
  ).

% delete(table,conds) : chopper le nom de la table, le nom de la colonne et la valeur de la colonne.
% Ensuite, maplist(maplist(retract),[[Liste_des_valeurs],row(Table,_)], NewList)
