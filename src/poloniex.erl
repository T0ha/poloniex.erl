%%%-------------------------------------------------------------------
%%% @author ins
%%% @copyright (C) 2019, ins
%%% @doc
%%%
%%% @end
%%% Created : 2019-12-12 13:42:45.336296
%%%-------------------------------------------------------------------
-module(poloniex).

-behaviour(cryptoring_amqp_exchange).

%% API
-export([
         start_link/0
        ]).

%% cryptoring_amqp_exchange callbacks
-export([buy/3
        ,sell/3
        ,balances/0
        ,subscribe_pair/1
        ,open_orders/0
        ,asks/2
        ,bids/2
        ,cancel_order/2
        ]).

-define(SERVER, ?MODULE).

-include("exchange.hrl").

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
    cryptoring_amqp_exchange:start_link(?MODULE).

%%%===================================================================
%%% cryptoring_amqp_exchange callbacks
%%%===================================================================
buy(Pair, Price, Amount) ->
    Resp = poloniex_http_private:buy(Pair, Price, Amount),
    cryptoring_amqp_log:log(<<"order">>, Resp).

sell(Pair, Price, Amount) ->
    Resp = poloniex_http_private:sell(Pair, Price, Amount),
    cryptoring_amqp_log:log(<<"order">>, Resp).

balances() ->
    case poloniex_http_private:balances() of
        #{<<"error">> := E} ->
            lager:warning("Error getting balancies: ~p", [E]),
            Resp = #{
              <<"error">> => iolist_to_binary(io_lib:format("~p", [E])),
              <<"method">> => <<"balancies">>
             },
            cryptoring_amqp_log:log(<<"error">>, Resp),
            Resp;
        Balancies ->
            maps:map(fun(_Coin, Data) when is_map(Data) ->
                             maps:map(fun(_K, V) ->
                                              binary_to_float(V)
                                      end,
                                      Data);
                        (_K, V) -> V
                     end,
                     Balancies)
    end.

subscribe_pair(Pair) ->
    poloniex_pair_sup:add_pair(Pair),
    poloniex_ws:subscribe(Pair).

open_orders() ->
    case poloniex_http_private:open_orders() of
        #{<<"error">> := E} ->
            lager:warning("Error getting open_orders: ~p", [E]),
            Resp = #{
              <<"error">> => iolist_to_binary(io_lib:format("~p", [E])),
              <<"method">> => <<"open_orders">>
             },
            cryptoring_amqp_log:log(<<"error">>, Resp),
            Resp;
        Orders ->
            maps:fold(fun(_Pair, [], Acc) -> Acc;
                         (Pair, Ords, Acc) when is_list(Ords) ->
                              [#{<<"pair">> => Pair
                                ,<<"direction">> => Direction
                                ,<<"price">> => binary_to_float(Price)
                                ,<<"amount">> => binary_to_float(Amount)
                                ,<<"total">> => binary_to_float(Total)
                                ,<<"id">> => Id
                                ,<<"timestamp">> => datetime_to_ts(TS)
                                } || #{<<"type">> := Direction
                                      ,<<"rate">> := Price
                                      ,<<"amount">> := Amount
                                      ,<<"orderNumber">> := Id
                                      ,<<"total">> := Total
                                      ,<<"date">> := TS
                                      } <- Ords] ++ Acc;

                        (_K, _V, Acc) -> Acc
                      end,
                      [],
                      Orders)
    end.

asks(Pair, Limit) ->
    try poloniex_pair_srv:asks(Pair, Limit) of
        Result ->
            [#{<<"price">> => Price
              ,<<"amount">> => Amount
              } || {Price, Amount} <- Result]
    catch
        _:_ -> 
            []
    end.
            
bids(Pair, Limit) ->
    try poloniex_pair_srv:bids(Pair, Limit) of
        Result ->
            [#{<<"price">> => Price
              ,<<"amount">> => Amount
              } || {Price, Amount} <- Result]
    catch
        _:_ -> 
            []
    end.

cancel_order(_Pair, OrderId) ->
    Resp = poloniex_http_private:cancel_order(OrderId),
    cryptoring_amqp_log:log(<<"cancel_order">>, Resp#{<<"origOrderId">> => OrderId}).


datetime_to_ts(<<Y:4/bytes, "-", M:2/bytes, "-", D:2/bytes, " ", H:2/bytes, ":",MM:2/bytes,":",SS:2/bytes>>) ->
    Secs = calendar:datetime_to_gregorian_seconds({{binary_to_integer(Y)
                                     ,binary_to_integer(M)
                                     ,binary_to_integer(D)
                                     }, 
                                     {binary_to_integer(H)
                                     ,binary_to_integer(MM)
                                     ,binary_to_integer(SS)
                                     }}) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Secs * 1000.
