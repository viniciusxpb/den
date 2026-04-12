---
name: No auto-commit
description: User prefers to review diffs before committing — never commit automatically
type: feedback
---

Never commit code changes automatically. The user wants to review diffs themselves before any commit.

**Why:** User explicitly said "não commite ok claude? eu gosto de ver os difs"

**How to apply:** Implement and test changes, but stop before `git commit`. If the user asks for a commit explicitly, then proceed.
