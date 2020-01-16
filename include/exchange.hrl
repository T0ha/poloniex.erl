-include_lib("amqp_client/include/amqp_client.hrl").

-type symbol() :: binary().

-record(pair, {
          base :: binary(), % That is `quontity` of pair
          quote :: binary() % That is `price` of pair
         }).

-record(depth, {
          direction :: bid | ask,
          price :: float(),
          pair :: #pair{}
         }).

