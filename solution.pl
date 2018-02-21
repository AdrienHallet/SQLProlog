% We begin our Great Adventure !

/* PrintElems(+List)
 * Prints the elements of the @List,
 * one element per line
 *
 */ 
printElems([]).
printElems([Head|Tail]) :- 
    writeln(Head), 
    printElems(Tail).

tables :- 
    findall(X, table(X,_,_), List), % find all tables
    printElems(List).               % display them

tables(Tables) :- 
    findall(X, table(X,_,_), Tables).

create(Table, Cols) :- 
    length(Cols, Length),               % get the column count
    assert(table(Table, Cols, Length)). % add the table, columns and column count

cols(Table, Cols).

rows(Table).

insert(Table, Row).

drop(Table).

delete(Table).

selec(TableOrTables, Selectors, Conds, Projection).
