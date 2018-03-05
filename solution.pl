/* LSINF2335 - Project 1
 * Group 4 :
 *  - HALLET, Adrien
 *  - RUCQUOY, Alexandre
 */

:- dynamic table/3.
:- dynamic row/2.

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
    
    % Trying to create table without columns
    Type = empty_cols, !,
    string_concat("You are trying to create the table ", Var, Temp),
    string_concat(Temp, " without any column ... And you cannot do that.", Message);
    
    Type = duplicate_column, !,
    Message = "Columns names must be unique";

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
    
/* is_set(+Lst)
 *
 * True if @Lst has no duplicates,
 * False otherwise
 */
is_set(Lst) :-
    setof(X, member(X, Lst), Set),
    length(Lst, N),
    length(Set, N).

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
    length(Cols, Length),
    Length =< 0,
    !, build_error_message(empty_cols, Table, Message),
    throw(Message);
    
    not(is_set(Cols)),
    !, build_error_message(duplicate_column, Table, Message),
    throw(Message);
    
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
    tables(Tables),
    (
        member(Table,Tables)
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
    tables(Tables),
    (
        member(Table,Tables)
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
    tables(Tables),
    (
        member(Table,Tables)
        ->
        selec(Table, *, Conds, ResOfSelec),
        arg(2,ResOfSelec,Val),
        !,retractall(row(Table,Val))
        ;
        build_error_message(no_table,Table,Message),
        !,throw(Message)
    ).

/* get_full_cols(+Tables, -RawColumns, -Columns)
 *
 * Write all @Tables columns in @Columns
 */
get_full_cols([Table], RawColumns, Columns) :-
  cols(Table, CurColList),
  append(RawColumns, CurColList, Result),
  flatten(Result, Columns).
  
get_full_cols([HTable|TTable], RawColumns, Columns) :-
  cols(HTable, CurColList),
  append(RawColumns, CurColList, NewRawColumns),
  !,get_full_cols(TTable, NewRawColumns, Columns).

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
% Base case (consumed everything)
parse_selectors([],Selectors, ParsedSelectors) :-
  % We emptied the table list and we flatten it 
  !,flatten(Selectors, ParsedSelectors).

%Working case, end of table list and multiselect
parse_selectors([Htable|_], [HSelector|TSelector], ParsedSelectors) :-
  maplist(assert_full_col_name(Htable), [HSelector|TSelector], RawParsedSelectors),
  !,flatten(RawParsedSelectors,ParsedSelectors).
  
%Working case, table list and single select (star)
parse_selectors(Table, _, ParsedSelectors) :-
  is_list(Table),  
  !,get_full_cols(Table, _, ParsedSelectors).

%Working broad case
parse_selectors(Table, Selectors, ParsedSelectors) :-
  (
    is_list(Selectors)
    -> % Selection
      maplist(assert_full_col_name(Table), Selectors, ParsedSelectors)
    ;  % All columns (*)
      cols(Table, RawParsedSelectors),
      %cols(Table, RawParsedSelectors),
      maplist(assert_full_col_name(Table), RawParsedSelectors, ParsedSelectors)
  ).

/* selec(+TableOrTables, +Selectors, +Conds, -Projection)
 * @TableOrTables   : The table(s) to select
 * @Selectors       : The column(s) to select
 * @Conds           : The conditions to apply on our selection
 * @Projection      : The results of the selection
 */
selec(TableOrTables, Selectors, Conds, Projection) :-
  !, parse_selectors(TableOrTables, Selectors, ParsedSelectors),
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
      maplist(row, Table,TempRow),            % X-join the tables
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
 */
condition_loop([], _, _).   % base case
condition_loop([H|T], Columns, Row) :-
  (First, Remain) = H, %Unwrap contiguous conditions
  condition_loop([First,Remain], Columns, Row);

  build_tree(H, Columns, Row, Tree), %Build the condition tree
  evaluate_tree(Tree,[], Condition), %Parse the condition tree
  FunCondition=..Condition,          %Create condition from tree
  !,FunCondition,                    %check condition
  condition_loop(T, Columns, Row). % Condition is valid, goto next condition

/* build_tree(+FullCond, +Columns, +Row, -Tree)
 * Build a linear tree in @Tree from the @FullCond on @Columns
 * and @Row
 * > Helper method for condition_loop to create the tree
 */
build_tree(FullCond, Columns, Row, Tree) :-
  FullCond=..CondList,
  get_atomic(CondList, [], R),
  replace_by_atom(R, Columns, Row, [], Tree).

/* evaluate_tree(ToEval, Stock, FinalCondition)
 * @ToEval         : !MUST! be a linear tree formed with build_tree
 * @Stock          : !MUST! be a linear tree formed with build_tree
 * @FinalCondition : The final condition to check
 *
 * Evaluate a linear tree formed with build_tree.
 * You can call it either by putting everythink in ToEval or Stock,
 * Or by splitting them, no one cares, the predicate is smart enough
 * and will figure itself what to do.
 */
evaluate_tree(ToEval, Stock, FinalCondition) :-
  length(ToEval, 3),                            %If we can functorize
  (length(Stock,0)                              %And tree is empty
  -> ToEval = FinalCondition                    %We have our final expression
  ;                                             %But if tree was not empty
  !, Condition=..ToEval,                        %Evaluate current expression
  !, append(Stock, [Condition], NewTree),       %And put it with the stock
  !, evaluate_tree(NewTree, [], FinalCondition) %For next expression
  );
  
  length(ToEval, N),                            %We do not have a 3-tuple
  !,N > 2,                                      %Make sure we don't unwrap too much
  !,ToEval=[H|T],                               %Unwrap the expression
  !,append(Stock, [H], NewStock),               %Put the rest in stock
  !,evaluate_tree(T, NewStock, FinalCondition). %Try to evaluate again
    

/* replace_by_atom(+RawLinearTree, Columns, Row, [], Replaced)
 * With @Columns containing the selection columns and 
 * @Row the current Row, replace each non-atomic value
 * by the correct atomic value in @Row and put the result
 * in @Replaced.
 * Helper method for build_tree
 */
replace_by_atom([], _, _, Builder, Replaced):- %Base case, work has ended
  Replaced = Builder.                          %Return the value
replace_by_atom([H|T], Columns, Row, Builder, Replaced):-
  atomic(H),                                   %Already an atom
  !, append(Builder, [H], Result),
  !, replace_by_atom(T, Columns, Row, Result, Replaced);
  
  nth0(ColumnIndex, Columns, H),               %Replace column name by value
  !, nth0(ColumnIndex, Row, Atom),
  !, append(Builder, [Atom], Result),
  !, replace_by_atom(T, Columns, Row, Result, Replaced);
  
  !,is_list(H),                                %If neither atom nor column
  !, append(Builder, [H], Result),             %Then must be list for predicate
  !, replace_by_atom(T, Columns, Row, Result, Replaced).
  
/* get_atomic(+RawList, [], -Atomized)
 * 
 * helper method for the linear tree to
 * format the atoms in @RawList into
 * a usable list in @Atomized.
 * Only replace non-expression atoms
 * (to replace expression atoms, see
 * "replace_by_atom" above)
 */
get_atomic([], Builder, Atomized):- %Base case, list is now empty
    Atomized = Builder.             %We have our answer
get_atomic([H|T], Builder, Atomized) :- %Worker case, list not empty
  (atomic(H);H= +X;H=_/_;is_list(H)), %H is of the type we're looking for
  (H = +X                             %Remove the plus notation
    -> Add=_/X
    ; Add=H
  ),
  !, append(Builder, [Add], Result),
  !,get_atomic(T, Result, Atomized);
  
  !, 
  H=..Functor,                        %We could do nothing, must be a functor
  !, get_atomic(Functor, Builder, Atomized).
  
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
