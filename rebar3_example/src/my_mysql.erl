-module(my_mysql).
-compile([do_mysql/2]).
-export([do_mysql/2]).

do_mysql(Name, Info) ->
    {ok, MysqlPid} = mysql:start_link(
                  [{host, "localhost"}, 
                   {user, "root"},
                   {password, "baifachuan"}, 
                   {database, "mydb"}
                  ]
                ),

     ok = mysql:query(
           MysqlPid, 
           "INSERT INTO people (name, info) VALUES (?, ?)", [Name, Info]
        ),

     {ok, ColumnNames, Rows} = mysql:query(
                MysqlPid, 
                <<"SELECT * FROM people">>),

     io:format("ColumnNames: ~p~nRows: ~p~n", [ColumnNames, Rows]).