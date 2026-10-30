
- **Never commit or push without explicit permission from the user.** Do not commit as a "convenience" after completing a task. Wait to be asked.

- When fixing a bug, **we first create a test that replicates the bug, and then investigate and fix it.**

- **Never supply a default value (zero, empty list, `None`, fallback constant) where real data should exist.** If a value is missing, that is a bug —  crash immediately. A silent default hides the bug and costs days of debugging later.

- **No type coercion to "make it work".** Never cast, clamp, `int()`, `float()`, `np.nan_to_num()`, or otherwise massage a value into the expected type/range to keep code running. If the type or value is wrong, something is broken upstream — find and fix it.

- **No skipped tests.** Never use `pytest.skip()`, `@pytest.mark.skip`, or conditional skips for missing assets, missing config. If a test can't run, it must fail, not silently disappear from the results. **Exception**: unavailable hardware.

- **No breaking interfaces.** Understand how changed code works in the context of the whole project. If the interface changed -- you need to write tests for the parts working together.

- **Write designated black-box contract tests.** Check that the contract is valid. Your task is to find bugs, not mask them. Be critical.

- **Follow the test-driven development practice.** When implementing a new feature first write tests, then feature code, then mentally check that tests are thorough, critical, and adversarial, then see if the feature code can be simplified.

- **Write short unit-tests, not large integration tests.** Tests should be short and fast, taking less than 10 secs.

- **Tests should be critical, adversarial, not parroting the implementation.**

- **Minimize time spent on running tests.** Run only relevant tests. Use relevant tools to run tests in parallel.

- If a test fails, do not claim the task is complete. State the failure and investigate.

- **Be thorough.** Before claiming done, read the final diff end-to-end and make sure that there's no silent regressions, broken contracts, swallowed mistakes.

- **Report observed results, not intended ones.** Never say "should work", "this will fix it", or "the tests should pass" —  run them and report what actually happened. If you didn't run something, say "I did not run X".

- **Flag suspicious code.** If code you're modifying or depending on is dead, duplicated, or needlessly tangled, mention it in one line at the end (max 3, only if verified, e.g. grep shows no callers). Do not refactor it unless asked.
