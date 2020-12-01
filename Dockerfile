FROM erlang:22.3.2-alpine AS builder
COPY . /root/poloniex
WORKDIR /root/poloniex
RUN apk add git bash
RUN rebar3 do compile, release

FROM alpine:latest
WORKDIR /root
RUN apk add ncurses
RUN mkdir ssl
COPY --from=builder /root/poloniex/priv/ssl/ca_certificate.pem ./ssl
COPY --from=builder /root/poloniex/priv/ssl/client_certificate.pem ./ssl
COPY --from=builder /root/poloniex/priv/ssl/client_key.pem ./ssl
COPY --from=builder /root/poloniex/_build/default/rel/poloniex .
CMD ./bin/poloniex console
