trellang Changelog
===================

This project adheres to semantic versioning. Notable changes for each release are listed below.

## v1.2.0 — Unreleased

- Credentials-aware API:
  - Added Creds-first arities for all public APIs (maps: `#{trello_key, trello_token}`), legacy arities delegate to Creds variants.
  - Simplified credentials plumbing; `get_credentials/0` returns a map; helpers pattern-match and fail fast internally.
- Board mirror server:
  - New `trello_board_server` `gen_server` that mirrors a board with `refresh/1`, optional periodic refresh, and delegates for `create_card/3`, `update_card/3`, `add_check_item/4`.
- Reliability:
  - Broadened TLS SNI fallback handling in HTTP helpers.
  - Increased server call timeouts in CT to accommodate network variance.
- Documentation:
  - README updates: explicit credentials usage and board mirror quickstart; restored card, custom-field and board-dump examples.
  - Documented local release publication, protected CI credentials and version consistency checks.
- Testing:
  - New CT suites: `trello_credentials_SUITE` and `trello_board_server_SUITE`.
  - Added isolated, board-scoped test resource cleanup and offline regression tests.
  - Added release metadata, package and documentation checks; protected integration CI keeps raw credential-bearing logs private.
  - Corrected Dialyzer's OTP dependency setup and removed unreachable board-server refresh branches without changing behavior.

### Compare
- [Unreleased changes since v1.1.0](https://github.com/stritzinger/trellang/compare/v1.1.0...HEAD)

## v1.1.0 — 2025-09-18

- Added checklist support:
  - Checklists: `list_checklists/1`, `create_checklist/2`, `rename_checklist/2`, `set_checklist_pos/2`.
  - Check items: `add_check_item/3`, `rename_check_item/3`, `set_check_item_state/3`, `set_check_item_pos/3`, `delete_check_item/2`, `set_check_item_due/3`, `assign_check_item_member/3`.
- Board dump enhancements:
  - `dump_board/2` supports `#{include_checklists => true}` to embed checklists per card.
- Documentation:
  - Examples for all checklist and check item APIs; expanded module highlights.
  - README configuration clarified (application env keys, `dev.config` example, `application:set_env/3` usage).
- Testing & internals:
  - New Common Test suite for checklists and check items
  - Added HTTP DELETE helper with retry/backoff (429/5xx) and jitter.

### Compare
- [v1.1.0 changes since v1.0.0](https://github.com/stritzinger/trellang/compare/v1.0.0...v1.1.0)
- [Hex package](https://hex.pm/packages/trellang/1.1.0) · [Versioned documentation](https://hexdocs.pm/trellang/1.1.0/)

## v1.0.0 — 2025-09-08

- Initial release on Hex with a focused, board‑agnostic Trello client.
- Cards:
  - `me/0`, `get_card/1`, `create_card/2`, `update_card/2` and ergonomic helpers (`set_name/2`, `set_desc/2`, `set_due/2`, `clear_due/1`, `set_pos/2`).
  - Labels: resolve by color and add (`get_label_id_by_color/2`, `add_label/2`, `add_label_by_color/3`).
  - Members: resolve by username and add (`list_board_usernames/1`, `get_member_id_by_username/2`, `add_member/2`, `add_member_by_username/3`).
- Custom fields:
  - `list_custom_fields/1`, `set_custom_field_text/3`, `set_custom_field_date/3`, `set_custom_field_checkbox/3`, `get_card_with_custom_fields/1`.
- Discovery:
  - `list_lists/1`, `list_cards_for_list/2`, `dump_board/2`.
- Reliability & architecture:
  - Retry/backoff with jitter (`rand:uniform/1`) for GET/POST/PUT.
  - OTP 27 `json` module for JSON; only credentials read from application env (`trellang.trello_key`, `trellang.trello_token`).
  - ExDoc configured; Hex publishing and relx release setup.

### Links
- [v1.0.0 source](https://github.com/stritzinger/trellang/tree/v1.0.0)
- [Hex package](https://hex.pm/packages/trellang/1.0.0) · [Versioned documentation](https://hexdocs.pm/trellang/1.0.0/)


