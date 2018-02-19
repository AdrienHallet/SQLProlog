:- dynamic table/3.
:- dynamic row/2.
    
table(persons,  [persons/id, persons/first, persons/last, persons/age, persons/city], 5).
table(friends,  [friends/a, friends/b], 2).
table(cities,   [cities/name, cities/state, cities/temp], 3).

row(persons, [0, "Jeffrey", "Bowman", 30, "Daytona Beach"]).
row(persons, [1, "Lorena", "Michaels", 50, "Boardman"]).
row(persons, [2, "Joseph", "Mixson", 37, "Washington"]).
row(persons, [3, "Stewart", "Sullivan", 83, "Grand Island"]).
row(persons, [4, "Thomas", "Marshall", 75, "Houston"]).
row(persons, [5, "Tameka", "Escobedo", 25, "Orlando"]).
row(persons, [6, "Annette", "Marcy", 40, "Pittsburgh"]).
row(persons, [7, "David", "Smith", 36, "Orlando"]).
row(persons, [8, "Anthony", "Garcia", 20, "Daytona Beach"]).
row(persons, [9, "Lindsay", "Faught", 70, "Boardman"]).
row(persons, [10, "Claudette", "Neil", 32, "Washington"]).
row(persons, [11, "Betty", "Strickland", 47, "Atlanta"]).
row(persons, [12, "Richard", "Gordon", 47, "Dallas"]).
row(persons, [13, "Robert", "Evans", 69, "Atlanta"]).
row(persons, [14, "Candace", "Griffin", 61, "Pittsburgh"]).
row(persons, [15, "Sandra", "Smith", 67, "Dallas"]).

row(cities, ["Daytona Beach", "Florida", 22]).
row(cities, ["Boardman", "Oregon", 12]).
row(cities, ["Washington", "District of Columbia", 11]).
row(cities, ["Grand Island", "New York", 10]).
row(cities, ["Houston", "Texas", 20]).
row(cities, ["Orlando", "Florida", 23]).
row(cities, ["Dallas", "Texas", 17]).
row(cities, ["Pittsburgh", "Pennsylvania", 11]).
row(cities, ["Atlanta", "Georgia", 16]).

row(friends, [0, 1]).
row(friends, [0, 8]).
row(friends, [0, 12]).
row(friends, [0, 15]).
row(friends, [1, 9]).
row(friends, [1, 13]).
row(friends, [1, 14]).
row(friends, [2, 10]).
row(friends, [2, 13]).
row(friends, [4, 7]).
row(friends, [4, 12]).
row(friends, [5, 7]).
row(friends, [5, 14]).
row(friends, [5, 15]).
row(friends, [6, 12]).
row(friends, [6, 13]).
row(friends, [8, 12]).
