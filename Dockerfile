FROM erlang:22.3.2
COPY . /root/poloniex
WORKDIR /root/poloniex
RUN rebar3 release
CMD _build/default/rel/poloniex/bin/poloniex console
