%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2019, ins
%%% @doc
%%%
%%% @end
%%% Created : 2019-11-18 13:25:35.910979
%%%-------------------------------------------------------------------
-module(poloniex_http_private).

-behaviour(gen_server).
-include("poloniex.hrl").

%% API
-export([
         start_link/0,
         balances/0,
         open_orders/0,
         open_orders/1,
         order_status/1,
         buy/3,
         sell/3
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

balances() ->
    post(?BALANCES, []).

open_orders() ->
    open_orders(<<"all">>).

open_orders(Pair) ->
    post(?OPEN_ORDERS, [{"currencyPair", Pair}]).

order_status(OrderId) ->
    post(?ORDER_STATUS, [{"orderNumber", OrderId}]).

buy(Pair, Price, Amount) when is_float(Price) ->
    buy(Pair, float_to_bin(Price), Amount);
buy(Pair, Price, Amount) when is_float(Amount) ->
    buy(Pair, Price, float_to_bin(Amount));
buy(Pair, Price, Amount) ->
    post(?BUY, [
                {"rate", Price},
                {"amount", Amount},
                {"currencyPair", Pair}
               ]).

sell(Pair, Price, Amount) when is_float(Price) ->
    sell(Pair, float_to_bin(Price), Amount);
sell(Pair, Price, Amount) when is_float(Amount) ->
    sell(Pair, Price, float_to_bin(Amount));
sell(Pair, Price, Amount) ->
    post(?SELL, [
                 {"rate", Price},
                 {"amount", Amount},
                 {"currencyPair", Pair}
                ]).

post(Method, Args) ->
    case gen_server:call(?SERVER, {post, Method, Args}, infinity) of
        recurse ->
            post(Method, Args);
        {ok, Reply} ->
            Reply
    end.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Connection} = connect(),
    Key = application:get_env(poloniex, key, <<>>),
    Secret = application:get_env(poloniex, secret, <<>>),


    lager:info("Starting ~p", [?MODULE]),
    {ok, 
     #connection{
            connection = Connection,
            key = Key,
            secret = Secret,
            headers = [
                       {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
                       {<<"Key">>, Key}
                       ]
           }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({post, Method, Params},
            _From,
            #connection{
               connection = Connection,
               secret = Secret,
               headers = Headers
              } = State) ->
    QS = api_url(Method, Params),
    lager:debug("QS: ~p", [QS]),
    Ref = gun:post(Connection, ?PRIVATE_PATH, headers(QS, Headers, Secret), QS),
    Reply = handle_responce(Connection, Ref, gun:await(Connection, Ref)),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({gun_up, Connection, http}, #connection{connection = Connection} = State) ->
    lager:info("HTTP connected ~p", [Connection]),
    {noreply, State};
handle_info({gun_down, Connection, http, Reason, _KilledStreams},
            #connection{connection = Connection} = State) ->
    lager:info("Publick HTTP ~p disconnected with reason ~p, reconnecting", [Connection, Reason]),
    %{ok, NewConnection} = connect(),
    {noreply, State#connection{connection = Connection}};
handle_info({gun_down, Connection, http, Reason, _KilledStreams, _UnprocessedStreams},
            #connection{connection = Connection} = State) ->
    lager:info("Publick HTTP ~p disconnected with reason ~p, reconnecting", [Connection, Reason]),
    %{ok, NewConnection} = connect(),
    {noreply, State#connection{connection = Connection}};
handle_info({gun_responce, Connection, Ref, fin, Status, Headers},
            #connection{
               connection = Connection,
               ref = Ref,
               from = Pid
              } = State) ->
    lager:info("Empty HTTP responce recieved for ~p", [Ref]),
    gen_server:reply(Pid, <<>>),
    {noreply, State#connection{from = undefined, ref = undefined}};
handle_info({gun_responce, Connection, Ref, nofin, Status, Headers},
            #connection{
               connection = Connection,
               ref = Ref,
               from = Pid
              } = State) ->
    lager:info("HTTP responce recieved for ~p", [Ref]),
    {noreply, State};
handle_info({gun_data, Connection, Ref, nofin, Status, Headers},
            #connection{
               connection = Connection,
               ref = Ref,
               from = Pid
              } = State) ->
    lager:info("HTTP responce recieved for ~p", [Ref]),
    {noreply, State};
handle_info(_Info, State) ->
    lager:warning("Wrong message: ~p in module ~p state ~p", [_Info, ?MODULE, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to terminate. It should be the opposite of Module:init/1 and do any necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
connect() ->
    lager:info("Connecting to ~s", [?HOST]),
    gun:open(?HOST, 443, #{protocols => [http]}).

api_url(Endpoint) ->
    api_url(Endpoint, []).

api_url(Endpoint, Params) ->
    Query = uri_string:compose_query([
                                      {"command", Endpoint},
                                      {"nonce", integer_to_binary(erlang:system_time())}
                                      | Params]),
    Query.

headers(QS, Headers, Secret) ->
    [{<<"Sign">>, sign(QS, Secret)} | Headers].

sign(Data, Secret) ->
    bin_to_hexstr(crypto:hmac(sha512, Secret, Data)).

bin_to_hexstr(Data) ->
        lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Data]).

float_to_bin(Float) ->
    float_to_binary(Float, [{decimals, 10}, compact]).

handle_responce(Connection, Ref, {response, nofin, _Code, _Headers}) ->
    handle_body(gun:await_body(Connection, Ref));
handle_responce(_Connection, _Ref, {error, {stream_error, closed}}) ->
    recurse;
handle_responce(_Connection, _Ref, {error, {closed, _}}) ->
    recurse;
handle_responce(_Connection, _Ref, {error, timeout}) ->
    recurse;
handle_responce(_Connection, _Ref, {error, E}) ->
    {ok, #{<<"error">> => iolist_to_binary(io_lib:format("~p", [E]))}}.


handle_body({ok, Body}) ->
    Json = try
               jsx:decode(Body, [return_maps])
           catch
               _:_ ->
                   #{<<"error">> => <<"Malformed Json">>}
           end,
    {ok, Json};
handle_body({error, Error}) ->
    Json = #{<<"error">> => Error},
    {ok, Json}. 
