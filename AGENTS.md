# AGENTS.md

このファイルは、`/home/arle/workspace/common-lisp/list-validate` で作業するエージェント向けのローカル運用メモです。

## 言語

- ユーザー向けの応答は日本語で行う。

## リポジトリ概要

- システム名: `list-validate`
- 主なコード:
  - `src/package.lisp`
  - `src/validate.lisp`
  - `src/validator-rules.lisp`
  - `src/parser-rules.lisp`
- テスト:
  - `tests/validate.lisp`
- ビルド/テスト:
  - `make test`

## 実装方針

- 公開インターフェース（`src/package.lisp` の `:export`）を優先して維持する。
- ルール評価順は「前方から順番」を守る。
- 既存のエラー条件:
  - `validation-error`
  - `validation-parse-error`
  の `field/value/rules` を壊さない。
- 後方互換を入れる場合は、ユーザーの明示要望があるときのみ行う。

## 変更時のチェック

1. 変更後に `make test` を実行する。
2. 失敗時は原因と影響範囲を明示する。
3. 新機能追加時は公開インターフェース経由のテストを優先する。

## スタイル

- 既存の命名・マクロ設計に合わせる。
- 不要なグローバル状態を増やさない。
- 可能な範囲で重複を減らし、共通ヘルパーに寄せる。
