% We begin our Great Adventure !

/* build_error_message(+Type, +Var, -Message)
 * Constructs an error message of the given @Type
 * With the possible help of the information @Var
 * And put it inside the @Message
 */
build_error_message(Type, Var, Message) :-
    % The table does not exist
    Type = no_table,
    string_concat("Table ", Var, Temp),
    string_concat(Temp, " does not exist", Message);
    
    % The table already exist
    Type = already_exists,
    string_concat("Table ", Var, Temp),
    string_concat(Temp, " already exists in database", Message);
    
    % The exception is unknown
    throw("Incexception : unexpected exception").

/* print_elems(+List)
 * Prints the elements of the @List,
 * one element per line
 *
 */ 
print_elems([]).
print_elems([Head|Tail]) :- 
    writeln(Head), 
    print_elems(Tail).

tables :- 
    findall(X, table(X,_,_), Tables), % find all tables
    print_elems(Tables).               % display them

tables(Tables) :- 
    findall(X, table(X,_,_), Tables).

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

insert(Table, Row).

drop(Table).

delete(Table).

selec(TableOrTables, Selectors, Conds, Projection).
