FROM elixir:1.8.1

ARG MIX_ENV=dev

COPY . /app
WORKDIR /app

RUN mix local.hex --force && \
    mix local.rebar --force && \
    MIX_ENV=${MIX_ENV} mix deps.get && \
    MIX_ENV=${MIX_ENV} mix deps.compile && \
    MIX_ENV=${MIX_ENV} mix compile


