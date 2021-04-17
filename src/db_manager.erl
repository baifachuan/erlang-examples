%% Connect (ssl is optional)
{ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "foo"},
                              {password, "hello"}, {database, "test"},
                              {ssl, [{server_name_indication, disable},
                                     {cacertfile, "/path/to/ca.pem"}]}]),

%% Select
{ok, ColumnNames, Rows} =
    mysql:query(Pid, <<"SELECT * FROM mytable WHERE id = ?">>, [1]),

%% Manipulate data
ok = mysql:query(Pid, "INSERT INTO mytable (id, bar) VALUES (?, ?)", [1, 42]),

%% Separate calls to fetch more info about the last query
LastInsertId = mysql:insert_id(Pid),
AffectedRows = mysql:affected_rows(Pid),
WarningCount = mysql:warning_count(Pid),

%% Mnesia style transaction (nestable)
Result = mysql:transaction(Pid, fun () ->
    ok = mysql:query(Pid, "INSERT INTO mytable (foo) VALUES (1)"),
    throw(foo),
    ok = mysql:query(Pid, "INSERT INTO mytable (foo) VALUES (1)")
end),
case Result of
    {atomic, ResultOfFun} ->
        io:format("Inserted 2 rows.~n");
    {aborted, Reason} ->
        io:format("Inserted 0 rows.~n")
end,

%% Multiple queries and multiple result sets
{ok, [{[<<"foo">>], [[42]]}, {[<<"bar">>], [[<<"baz">>]]}]} =
    mysql:query(Pid, "SELECT 42 AS foo; SELECT 'baz' AS bar;"),

%% Graceful timeout handling: SLEEP() returns 1 when interrupted
{ok, [<<"SLEEP(5)">>], [[1]]} =
    mysql:query(Pid, <<"SELECT SLEEP(5)">>, 1000),

%% Close the connection
mysql:stop(Pid).