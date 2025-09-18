trellang Changelog
===================

This project adheres to semantic versioning. Notable changes for each release are listed below.

## v1.1.0

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
- v1.1.0 vs v1.0.0: `https://github.com/stritzinger/trellang/compare/v1.0.0...v1.1.0`

## v1.0.0

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
- v1.0.0 tag: `https://github.com/stritzinger/trellang/releases/tag/v1.0.0`


