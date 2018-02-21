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

create(Table, Cols) :- % Todo : check if two cols do not have the same name
    tables(Tables),
    ( 
    member(Table, Tables) 
        -> 
        !, throw("Table already exists")
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
        !, throw("Specified Table does not Exist")
    ).

rows(Table).

insert(Table, Row).

drop(Table).

delete(Table).

selec(TableOrTables, Selectors, Conds, Projection).
