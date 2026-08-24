---
name: post-task-learning-review
description: Review substantial completed tasks for durable lessons and store only high-value reusable guidance.
when-to-use: Apply before the top-level agent finishes a substantial task, especially after debugging, implementing a feature, validating a workflow, or preparing a pull request.
activation: always
user-invocable: false
disable-model-invocation: true
---

# Post-task learning review

Before the final response, review the completed work for durable, actionable
lessons.

Consider:

- surprising failure causes;
- repository conventions;
- reliable validation procedures;
- recurring user preferences;
- approaches that failed and should not be repeated.

Search existing learned skills before creating anything. Update an existing
skill when possible. Create a skill only when the lesson is reusable,
non-obvious, supported by concrete evidence, and likely to change future
behavior.

Prefer the narrowest applicable database scope. Do not store ordinary task
facts, summaries, temporary state, speculation, or guidance already captured
in repository instructions.

Create at most two learned-skill mutations per task. If there is no meaningful
learning, do nothing and do not mention the review.
