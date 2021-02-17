-spec start(
    StartType :: typr:application_start_type(),
    StartArgs :: term()
) ->
    typr:application_start_return().

-spec stop(
    State :: term()
) ->
    Return :: term().
