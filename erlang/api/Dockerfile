FROM erlang:latest
LABEL authors="alejandrorodriguez"

WORKDIR /app

COPY . .

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o /usr/local/bin/rebar3 && \
    chmod +x /usr/local/bin/rebar3

RUN rebar3 compile

CMD ["rebar3", "shell"]