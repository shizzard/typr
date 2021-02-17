-spec init(
    Args :: term()
) ->
    {ok, State :: state()} |
    {ok, State :: state(), non_neg_integer() | hibernate} |
    {stop, Reason :: term()} |
    ignore.

-spec handle_call(
    Request :: typr:gen_server_generic_message(),
    From :: typr:gen_from(),
    State :: state()
) ->
    typr:gen_server_return(state()).

-spec handle_cast(
    Request :: typr:gen_server_generic_message(),
    State :: state()
) ->
    typr:gen_server_noreply(state()) |
    typr:gen_server_stop_noreply(state()).

-spec handle_info(
    Request :: typr:gen_server_generic_message(),
    State :: state()
) ->
    typr:gen_server_noreply(state()) |
    typr:gen_server_stop_noreply(state()).

-spec terminate(
    Reason :: term(),
    State :: state()
) ->
    typr:ok_return().

-spec code_change(
    OldVsn :: term(),
    State :: state(),
    Extra :: term()
) ->
    typr:ok_return(Ret :: state()).
