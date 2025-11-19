#!/bin/bash

echo "====== REPLAYLIST Fly Deploy Script ======"

# .env は backend 配下にある
ENV_PATH="./backend/.env"

if [ ! -f "$ENV_PATH" ]; then
  echo "ERROR: backend/.env が見つかりません。"
  exit 1
fi

echo "[1/3] Elm をビルドします"

# 既存 main.js 削除（Permission denied 対策）
rm -f ./frontend/main.js

# Elm ビルド（frontend へ移動して実行）
cd frontend
elm make src/Main.elm --output=main.js
cd ..

echo "[2/3] Fly Secrets を更新します"

# backend/.env の内容を読み込み、"KEY=VALUE" を一行ずつ処理
while IFS='=' read -r key value; do
  # 空行とコメントはスキップ
  if [[ -z "$key" ]] || [[ "$key" =~ ^# ]]; then
    continue
  fi

  # Apple の秘密鍵だけは特殊処理（p8 中身を渡す）
  if [[ "$key" == "APPLE_PRIVATE_KEY_PATH" ]]; then
    echo "  -> APPLE_PRIVATE_KEY_CONTENTS をセット"
    fly secrets set APPLE_PRIVATE_KEY_CONTENTS="$(cat backend/keys/appleAPILogin.p8)"
    continue
  fi

  echo "  -> $key をセット"
  fly secrets set "$key=$value"

done < "$ENV_PATH"

echo "[3/3] Fly Deploy を実行します"

fly deploy --remote-only

echo "====== Deploy 完了 ======"
