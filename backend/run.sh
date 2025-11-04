#!/usr/bin/env bash
set -euo pipefail

echo "🧹 Cleaning up old processes..."
pkill -f "ngrok.*replaylist" 2>/dev/null || true
lsof -ti :8080 | xargs kill -9 2>/dev/null || true
pkill -f "target/debug/replaylist" 2>/dev/null || true

echo "🚀 Starting backend..."
cargo run --quiet &
BACK_PID=$!

# サーバーの起動を待つ
until curl -s http://127.0.0.1:8080/health >/dev/null 2>&1; do
  sleep 0.5
done

echo "🌐 Starting ngrok tunnel..."
ngrok start replaylist --config "$HOME/.config/ngrok/ngrok.yml" &

trap "kill $BACK_PID; pkill -f ngrok" EXIT
wait
