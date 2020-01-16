%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2019, ins
%%% @doc
%%%
%%% @end
%%% Created : 2019-12-12 13:42:45.336296
%%%-------------------------------------------------------------------
-module(poloniex).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         best_bid_updated/2,
         best_ask_updated/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("exchange.hrl").

-record(state, {
          connection :: pid() | undefined,
          channel :: pid() | undefined,
          consumer_tag :: binary() | undefined
         }).

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

best_bid_updated(Pair, Price) ->
    lager:debug("New best bid for ~s: ~p", [Pair, Price]),
    gen_server:cast(?SERVER, {best, bid, Pair, Price}).

best_ask_updated(Pair, Price) ->
    lager:debug("New best ask for ~s: ~p", [Pair, Price]),
    gen_server:cast(?SERVER, {best, ask, Pair, Price}).

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
    DefaultParama = #amqp_params_network{},

    Settings = application:get_all_env(amqp_client),
    Host = proplists:get_value(host, Settings, DefaultParama#amqp_params_network.host),
    Port = proplists:get_value(port, Settings, DefaultParama#amqp_params_network.port),

    Username = proplists:get_value(username, Settings, DefaultParama#amqp_params_network.username),
    Password = proplists:get_value(password, Settings, DefaultParama#amqp_params_network.password),

    VHost = proplists:get_value(virtual_host, Settings, DefaultParama#amqp_params_network.virtual_host),
    Heartbeat = proplists:get_value(heartbeat, Settings, DefaultParama#amqp_params_network.heartbeat),

    AmqpParams = #amqp_params_network{
                    host = Host,
                    port = Port,
                    username = Username,
                    password = Password,
                    virtual_host = VHost,
                    heartbeat = Heartbeat
                   },
    {ok, Connection} = amqp_connection:start(AmqpParams),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    OrderExchange = #'exchange.declare'{
                       exchange = <<"OrderBookTop">>,
                       type = <<"topic">>
                       %nowait = true
                      },
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, OrderExchange),

    Queue = #'queue.declare'{queue = <<"poloniex">>},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Queue),

    Subscribe = #'basic.consume'{queue = <<"poloniex">>},
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:call(Channel, Subscribe),

    
    {ok, #state{
           connection = Connection,
           channel = Channel,
           consumer_tag = Tag
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
handle_cast({best, Direction, Pair, Price}, 
            #state{
               connection = Connection,
               channel = Channel
            } = State) ->
    Data = #{
      <<"exchange">> => poloniex,
      <<"pair">> => Pair,
      <<"direction">> => Direction,
      <<"price">> => Price
      %<<"amount">> => 
      },
    JSON = jsx:encode(Data),

    Publish = #'basic.publish'{
                 exchange = <<"OrderBookTop">>,
                 routing_key = Pair
                },
    Msg = #amqp_msg{payload = JSON},
    amqp_channel:cast(Channel, Publish, Msg),

    {noreply, State};
handle_cast(_Msg, State) ->
    lager:warning("Wrong message: ~p in module ~p state ~p", [_Msg, ?MODULE, State]),
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
handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info(#'basic.cancel_ok'{}, State) ->
    {noreply, State};
handle_info({#'basic.deliver'{delivery_tag = Tag}, 
             #amqp_msg{
                props = #'P_basic'{
                           correlation_id = CID,
                           reply_to = Reply
                          },

                payload = Data
               }
            },
            #state{
               channel = Channel
              } = State) ->
    lager:info("AMQP reply ~p cid ~p message: ~p", [Reply, CID, Data]),

    Result = apply_api_method(jsx:decode(Data, [return_maps])),

    Response = #'basic.publish'{routing_key = Reply},
    Props = #'P_basic'{correlation_id = CID},
    Msg = #amqp_msg{props = Props, payload = jsx:encode(Result)},
    amqp_channel:cast(Channel, Response, Msg),

    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
    {noreply, State};
handle_info(_Info, State) ->
    lager:warning("Wrong message: ~p in module ~p state ~p", [_Info, ?MODULE, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
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
bin_to_pair(<<Quote:3/bytes, "_", Base:3/bytes>>) ->
    #pair{
       base = Base,
       quote = Quote
      }.
apply_api_method(#{<<"action">> := <<"subscribe">>,
                  <<"pair">> := Pair
                  }) ->
    poloniex_pair_sup:add_pair(Pair),
    poloniex_ws:subscribe(Pair);
apply_api_method(#{<<"action">> := <<"buy">>,
                  <<"price">> := Price,
                  <<"amount">> := Amount,
                  <<"pair">> := Pair
                  }) ->
    poloniex_http_private:buy(Pair, Price, Amount);
apply_api_method(#{<<"action">> := <<"sell">>,
                  <<"price">> := Price,
                  <<"amount">> := Amount,
                  <<"pair">> := Pair
                  }) ->
    poloniex_http_private:sell(Pair, Price, Amount);
apply_api_method(#{<<"action">> := <<"balances">>}) ->
    poloniex_http_private:balances();
apply_api_method(Data) ->
    lager:warning("Unknown method: ~p in module ~p", [Data, ?MODULE]).

