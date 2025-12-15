---
"aywson": patch
---

Add support for trailing comments in JSON operations

Implements getTrailingComment, setTrailingComment, and removeTrailingComment functions to handle trailing comments after fields. Updates all relevant operations (get, set, remove, modify, merge, sort, etc.) to preserve or manipulate trailing comments as appropriate. Enhances CLI and documentation to support and describe trailing comment features, and adds comprehensive tests for trailing comment behavior.
