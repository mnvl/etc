
- **Never commit or push without explicit permission from the user.** Do not commit as a "convenience" after completing a task. Wait to be asked.

- When investigating a bug, **we first create a test that replicates the bug, and then investigate and fix it.**

- **Never supply a default value (zero, empty list, `None`, fallback constant) where real data should exist.** If a value is missing, that is a bug — crash immediately. A silent default hides the bug and costs days of debugging later.

- **No type coercion to "make it work".** Never cast, clamp, `int()`, `float()`, `np.nan_to_num()`, or otherwise massage a value into the expected type/range to keep code running. If the type or value is wrong, something is broken upstream — find and fix it.

- **No skipped tests.** Never use `pytest.skip()`, `@pytest.mark.skip`, or conditional skips for missing assets, missing config, or "not available" hardware. If a test can't run, it must fail, not silently disappear from the results.

- **No breaking interfaces.** Understand how changed code works in the context of the whole project. If the interface changed -- you need to write tests for the parts working together.

- **Write designated black-box contract tests.** Check that the contract is valid. Your task is to find bugs, not mask them. Be critical.

- **Tests should be critical, adversarial, not parroting the implementation.**

- **Be thorough.** Before claiming done, read the final diff end-to-end and make sure that there's no silent regressions, broken contracts, swallowed mistakes.

- **Report observed results, not intended ones.** Never say "should work", "this will fix it", or "the tests should pass" — run them and report what actually happened. If you didn't run something, say "I did not run X".

- If a test fails after your change, do not claim the task is complete. State the failure, investigate, and either fix it or explain why the failure is unrelated with evidence.
