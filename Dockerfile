FROM rust:1.84 AS builder

WORKDIR /app

RUN apt-get update && apt-get install -y nodejs npm && \
    npm install -g elm && \
    rm -rf /var/lib/apt/lists/*

COPY . .

WORKDIR /app/frontend
RUN elm make src/Main.elm --output=main.js

WORKDIR /app/backend
RUN cargo build --release


FROM debian:bookworm-slim AS final

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates openssl && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app/backend

COPY --from=builder /app/backend/target/release/backend ./backend

COPY --from=builder /app/frontend /app/frontend

EXPOSE 8080

CMD ["./backend"]
