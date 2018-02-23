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
index_of([_|_], _, 0) :- !.
index_of([_|Tail], Column, Index) :-
  index_of(Tail, Column, SubIndex),
  !,
  SubIndex is Index+1.

/* replace(+Old, +New, +OldList, -NewList)
 * Replace the first occurence of @Old by @New
 * in @OldList and bind to @NewList
 */
replace(_, _, [], []).                      % base case
replace(Old, New, [Old|T1], [New|T2]) :-    % Head of list is value
    replace(Old, New, T1, T2).
replace(Old, New, [H|T1], [H|T2]) :-        % Pursue the search
    dif(H,Old),                             % Head is not the value
    replace(Old, New, T1, T2).

/* tables
 * List the tables on prompt
 */
tables :-
    findall(X, table(X,_,_), Tables), % find all tables
    print_elems(Tables).              % display them

/* tables(-Tables)
 * Bind the list of tables to @Tables
 */
tables(Tables) :-
    findall(X, table(X,_,_), Tables).

/* assert_full_col_name(+Table, +Col, -VerifiedCol)
 * Ensures that @VerifiedCol contains the full selector name
 * of @Col inside @Table
 *
assert_full_col_name(Table, Col, VerifiedCol) :-
  (
    _/_ = Col
    ->
      VerifiedCol = Col         % We already had the full name
    ;
      VerifiedCol = Table/Col   % Get the full name
  ).
*/

/* assert_full_col_name(+Table, +Col, -VerifiedCol)
 * Ensures that @VerifiedCol contains the full selector name
 * of @Col inside @Table
 */
assert_full_col_name(Table, Col, FullCol) :-

  (Col = +X
  ->
  FullCol = Table/X
  ;
      (
        Col = _/_
        ->
          FullCol = Col         % We already had the full name
        ;
          FullCol = Table/Col   % Get the full name
      )
  ).

/* create(+Table, +Cols)
 * Adds the @Table with columns @Cols to the database
 */
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

/* cols(+Table, -Cols)
 * Get the @Table's columns inside @Cols
 */
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

/* rows(+Table)
 * List the rows of @table in prompt
 */
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

/* insert(+Table, +Row)
 * Insert the @Row in @Table
 */
insert(Table, Row) :-
    % Check that we want to insert a list
    \+(is_list(Row)),
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
        % You want to insert too few/many columns
        build_error_message( 
            missing_columns,      
            (Length, Found),
            Message),
        !, throw(Message)
    )
    ;

    % Table does not exist
    build_error_message(no_table, Table, Message),
    !, throw(Message).

/*
 *
 */
drop(Table) :-
    (
        member(Table, _)
        ->
        retractall(row(Table,_)),
        retract(table(Table,_,_))
        ;
        build_error_message(no_table,Table,Message),
        !,throw(Message)
    ).
/*
 *
 */
delete(Table) :-
    (
        member(Table,_)
        ->
        retractall(row(Table,_))
        ;
        build_error_message(no_table,Table,Message),
        !,throw(Message)
    ).
/*
 *
 */
delete(Table,Conds) :-
    (
        member(Table,_)
        ->
        handle_conds(Conds,ListofArgs),
        retract(row(Table,ListofArgs))
        ;
        build_error_message(no_table,Table,Message),
        !,throw(Message)
    ).

/* handle_conds(+Conds, -ListofArgs)
 *
 */
handle_conds([],_).
handle_conds([H|T],ListofArgs) :-
    arg(2,H,V),
    handle_conds(T,[ListofArgs|V]).

/* parse_selectors(+Table(s),+Selector(s), -ParsedSelectors) 
 * @Table(s)       : can be list of tables or single table
 * @Selector(s)    : can be one or multiple selectors (also *)
 * @ParsedSelectors: gets bound to the full selectors
 *
 * The parser takes all the possible (assumed) values that we can
 * request on a selection, either with multiple or single selectors.
 * @ParsedSelectors will then contain the full, standardized selectors
 * to be used inside the selec predicate.
 */
parse_selectors([],Selectors, ParsedSelectors) :-
  % We emptied the table list and we flatten it 
  !,flatten(Selectors, ParsedSelectors).
  
parse_selectors([_|_], [], _).
  % We emptied the table and selector lists
  
/*parse_selectors([_|_], Selectors, ParsedSelectors) :- %Todo check weirdness
    Selectors = +X,
    ParsedSelectors = _/X.*/

parse_selectors([Htable|_], [HSelector|TSelector], ParsedSelectors) :-
  maplist(assert_full_col_name(Htable), [HSelector|TSelector], RawParsedSelectors),
  !,flatten(RawParsedSelectors,ParsedSelectors).
  
parse_selectors(Table, Selectors, ParsedSelectors) :-
  (
    is_list(Selectors)
    -> % Selection
      maplist(assert_full_col_name(Table), Selectors, ParsedSelectors)
    ;  % All columns (*)
      cols(Table, RawParsedSelectors),
      maplist(assert_full_col_name(Table), RawParsedSelectors, ParsedSelectors)
  ).

/* selec(+TableOrTables, +Selectors, +Conds, -Projection)
 * @TableOrTables   : The table(s) to select
 * @Selectors       : The column(s) to select
 * @Conds           : The conditions to apply on our selection
 * @Projection      : The results of the selection
 */
selec(TableOrTables, Selectors, Conds, Projection) :-
  parse_selectors(TableOrTables, Selectors, ParsedSelectors),
  !, selec_worker(TableOrTables, ParsedSelectors, Conds, Projection).

/* selec_worker(+Table(s), +Selector(s), +Cond(s), -Projection)
 * @Table(s)    : The table(s) to select
 * @Select(s)   : The column(s) to select, formatted
 * @Cond(s)     : The conditions to apply on our selection
 * @Projection  : The results of the selection
 *
 * The worker of the selection. It can be used instead of selec iif
 * @Selectors is standardized by the parser
 */
selec_worker(Table, Selectors, Conds, Projection) :-
  (
    Table = [_|_]
    -> % Multi-tables selection
      maplist(cols, Table, TempColumns),
      flatten(TempColumns,Columns),           % Get all viewed columns
      maplist(row, [persons,cities],TempRow), % X-join the tables
      flatten(TempRow, Row)                   % Get the rows
    ; % Single-table selection
      table(Table, RawColumns,_),
      maplist(assert_full_col_name(Table),RawColumns,Columns),
      row(Table, Row)
  ),

  condition_loop(Conds, Columns, Row),        % Only keep SAT(row, Conds)
  (is_list(Selectors)
  -> % refined selection
    selector(Columns, Selectors, Row, [], Selection),
    Projection = Selectors/Selection
  ;  % get-all (*) selection
    Projection = Selectors/Row
  ).


/* condition_loop(+Conditions, +Columns, +Row)
 * @Conditions : List of conditions to apply on selection
 * @Columns    : All the columns of the selected tables
 * @Row        : The row the condition is getting checked on
 *
 * Receiving a list of @Conditions over some or all of @Columns,
 * is true iif every condition is verified over @Row.
 *
 * Technical add-on :
 * The condition_loop loops over every handed condition, binding
 * the values found on desired column and comparing it to the
 * expected value according to the given operator.
 *
 * There is a special case when we give two columns (e.g. to join on
 * a multi-table selection), the compared columns are both evaluated
 * on their value found on Row (where the classis condition keeps a
 * constant for the comparison).
 */
condition_loop([], _, _).   % base case
condition_loop([H|T], Columns, Row) :-
  (First, Remain) = H,
  condition_loop([First,Remain], Columns, Row);

  H=..CondList, % Transform the condition into a uniform representation
  replace(+X,_/X,CondList,OrderList), % Convert the '+' notation to the standard table/column
  OrderList = [Operator, Column, ExpectedValue],
  nth0(ColumnIndex, Columns, Column), % Get the index of Column in Columns
  nth0(ColumnIndex, Row, FoundValue), % Get the value found at index on Row
  (_/_ = ExpectedValue
    -> % condition of type column1 = column2
      % Get index of ExpectedValue in Columns
      nth0(SecondColumnIndex, Columns, ExpectedValue),
      % Get value found at index on Row
      nth0(SecondColumnIndex, Row, SecondFoundValue),
      % Express the condition on comparable values
      Condition=..[Operator,FoundValue,SecondFoundValue]
    ; % condition of type column = value
      Condition=..[Operator,FoundValue,ExpectedValue]
  ),
  !,Condition, % Check the condition
  condition_loop(T, Columns, Row). % Condition is valid, goto next condition

/* selector(+Columns, +ColumnsKept, +Row, -Builder, -Projection)
 * @Columns     : The full names of the columns in a Row (<table>/<column>)
 * @ColumnsKept : The full names of the columns we want to keep (<table>/<column>))
 * @Row         : The Row on which to filter Columns
 * @Builder     : An accumulator to build the projection
 * @Projection  : Resulting projection of the selection on Row
 *
 * condition : order(Columns) = order(Row).
 * -> we consider that the order of the columns in Columns is the same as in Row
 *
 * Binds a new row to Projection, built from Row where every unwanted column has
 * been filtered out.
 */
selector([],_,_,Builder, Projection) :-  % base case, we consumed every selector
  Builder = Projection.
selector([CurCol|RemCol], ColumnsKept, [CurRow|RemRow], Builder, Projection) :-
  (
    member(CurCol, ColumnsKept)
    -> % we want to keep this column
      append(Builder, [CurRow], ExpandedBuilder),
      selector(RemCol, ColumnsKept, RemRow, ExpandedBuilder, Projection)
    ;  % we don't want this column
      selector(RemCol, ColumnsKept, RemRow, Builder, Projection)
  ).

% delete(table,conds) : chopper le nom de la table, le nom de la colonne et la valeur de la colonne.
% Ensuite, maplist(maplist(retract),[[Liste_des_valeurs],row(Table,_)], NewList)
