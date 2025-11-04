#!/bin/bash
set -e

# 例: backend と frontend が兄弟。出力先は frontend/main.js
echo "🧩 Building Elm..."
elm make src/Main.elm --output=main.js

echo "✅ Elm built to ./main.js"
