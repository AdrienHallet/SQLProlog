%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPERS

% Run the tests.
test :-
    run_tests(sql).

% 'clean' drop: call drop/1 and swallow any thrown exceptions
cdrop(Table) :-
    catch(drop(Table), _, true).

% Checks that both lists have the same content.
same_content(L1, L2) :-
    msort(L1, S),
    msort(L2, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS

:- begin_tests(sql).

% Test if create/1 works.
test(create, cleanup(cdrop(foo))) :-
    create(foo, [bar, baz]),
    tables(Tables),
    member(foo, Tables), !.

% Test if create/1 throws an error on dup table name.
test(create_redundant, [throws(_), cleanup(cdrop(foo))]) :-
    create(foo, [bar, baz]),
    create(foo, [bab, bac]).

% Test if the columns are properly recorded.
test(cols, cleanup(cdrop(foo))) :-
    create(foo, [baru, bazu]),
    cols(foo, Cols),
    assertion(Cols == [baru, bazu]).

% Test if drop/1 works.
test(drop) :-
    create(foo, [bar, baz]),
    drop(foo), !,
    tables(Tables),
    \+ member(foo, Tables).

% Test if drop/1 throws an error on unknown table name.
test(drop_inexistant, throws(_)) :-
    drop(foobar).

% Simple insert/2 & selec/4 test.
test(insert2, cleanup(cdrop(foo))) :-
    create(foo, [bar, baz]),
    insert(foo, [1, 2]),
    row(foo, Y),
    selec(foo, *, [], X/Y), !,
    assertion(X == [foo/bar, foo/baz]),
    assertion(Y == [1, 2]),
    insert(foo, [3, 4]),
    findall(Z, selec(foo, *, [], _/Z), Zs), !,
    assertion(Zs == [[1,2],[3,4]]).
    
% Simple delete/1 test.
test(delete1, cleanup(cdrop(foo))) :-
    create(foo, [bar, baz]),
    insert(foo, [1, 2]),
    insert(foo, [3, 4]),
    delete(foo),
    findall(Z, selec(foo, *, [], _/Z), Zs),
    assertion(Zs == []),
    tables(Tables),
    member(foo, Tables).

% Simple delete/2 test.
test(delete2, cleanup(cdrop(foo))) :-
    create(foo, [bar, baz]),
    insert(foo, [1, 2]),
    insert(foo, [3, 4]),
    delete(foo, [+bar = 1]),
    findall(Z, selec(foo, *, [], _/Z), Zs),
    assertion(Zs == [[3, 4]]).

% Test delete/2 using multiple conditions.
test(delete2_mult, cleanup(cdrop(foo))) :-
    create(foo, [bar, baz]),
    insert(foo, [1, 2]),
    insert(foo, [3, 4]),
    delete(foo, [+bar = 1, +baz = 2]),
    findall(Z, selec(foo, *, [], _/Z), Zs),
    assertion(Zs == [[3, 4]]).

% Test selec/4 with a single condition +col condition.
test(selec_cond1) :-
    selec(persons, *, [=(+id, 0)], _/Row1), !,
    assertion(Row1 == [0, "Jeffrey", "Bowman", 30, "Daytona Beach"]),
    selec(persons, *, [=(+first, "Lorena")], _/Row2), !,
    assertion(Row2 == [1, "Lorena", "Michaels", 50, "Boardman"]).

% Test selec/4 with a single table/col type condition.
test(selec_cond2) :-
    selec(persons, *, [=(persons/id, 0)], _/Row1), !,
    assertion(Row1 == [0, "Jeffrey", "Bowman", 30, "Daytona Beach"]),
    selec(persons, *, [=(persons/first, "Lorena")], _/Row2), !,
    assertion(Row2 == [1, "Lorena", "Michaels", 50, "Boardman"]).

% Test selec/4 with two +col conditions.
test(selec_cond_mult1) :-
    selec(persons, *, [(=(+age, 47), =(+city, "Atlanta"))], _/Row), !,
    assertion(Row == [11, "Betty", "Strickland", 47, "Atlanta"]).

% Test selec/4 with two table/col conditions.
test(selec_cond_mult2) :-
    selec(persons, *, [(=(persons/age, 47), =(persons/city, "Atlanta"))], _/Row), !,
    assertion(Row == [11, "Betty", "Strickland", 47, "Atlanta"]).

% Test selec/4 with a single projection and no conditions.
test(selec_proj) :-
    findall(Row, selec(persons, [+first], [], _/Row), Rows0),
    flatten(Rows0, Rows),
    assertion(Rows == ["Jeffrey", "Lorena", "Joseph", "Stewart", "Thomas",
        "Tameka", "Annette", "David", "Anthony", "Lindsay", "Claudette", "Betty",
        "Richard", "Robert", "Candace", "Sandra"]).

% Test selec/4 with projections and a condition.
test(selec_mult_proj) :-
    findall(Row, selec(persons, [+first, +last], [+age >= 70], _/Row), Rows),
    same_content(Rows, [
                     ["Stewart", "Sullivan"],
                     ["Thomas", "Marshall"],
                     ["Lindsay", "Faught"]]).

% Test selec/4 with a join.
test(selec_join) :-
    findall(Row, selec([persons, cities], [+id, cities/temp],
                       [+city = cities/name], _/Row), Rows),
    same_content(Rows, [
                     [0,  22],
                     [1,  12],
                     [2,  11],
                     [3,  10],
                     [4,  20],
                     [5,  23],
                     [6,  11],
                     [7,  23],
                     [8,  22],
                     [9,  12],
                     [10, 11],
                     [11, 16],
                     [12, 17],
                     [13, 16],
                     [14, 11],
                     [15, 17]]).

:- end_tests(sql).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
