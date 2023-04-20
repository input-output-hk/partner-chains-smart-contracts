# Developer guidelines

Following are the relevant developer guidelines for the project. If you have any suggestion, reach out to the team,
or create a PR.

- [Off-chain developer guideline](./offchain/CONTRIBUTING.md)
- [Haskell coding standards](./onchain/STANDARDS.md)

## GitHub rules

#### Before starting to work on an issue
- If there is no GitHub issue for your task, please create one
- Self-assign the issue if it wasn't assigned to you by the PM
- Set the status of the issue to `In progress`
- Put your name or github handle in the branch name (easier to handle stale branches), and some description of the issue and/or
  the issue number (e.g. `gergely/github-rules`)
- Feel free to create draft PRs for a work in progress branch to get CI feedback, but don’t forget to change its status to
  `Ready for review` when appropriate (and follow other rules below)
- Try to slice your work into smaller PRs to make the review process smoother

#### When creating a PR
- Every team member should be added to Pull Request reviewers (automatically added by CODEOWNERS)
- Each PR with a new feature must include tests
- In the description of the PR include at least the issue number, if the original issue has enough information about the feature.
  Try to help the reviewer understand your solution if it’s not obvious for some reason. Also mention any concerns or areas that
  you’re not sure about, so the reviewer can give extra attention to those areas.
- Connect the PR with an issue using the Development field (this will close the given issue when the PR is merged)
- Change the issue status to `In review`
- Do a self review, or check diffs before committing
- Check the CI after you push a change (it can take long sometimes, just remind yourself to check back on it later)
- If the PR doesn’t get reviewed in reasonable time (48h), please ping other team members (feel free to be annoying about it)
- If you addressed the reviewers comments, re-request review

#### When reviewing a PR
- Be nice
- If needed, checkout the code and give it a spin
- If the scope of the PR is too big or too complex, a live review session could be more effective
- You can utilise flags such as `[nits]` if your suggestion is not really important (stylistic issues etc.)

#### Before merging a PR
- PM should merge, once there are enough approvals, unless the PR is urgent or members agreed otherwise
- If the PM is absent, someone else from the team should be appointed to handle PR merges
- At least one, but ideally all reviewers, should approve a PR before merging (for example if the changes are trivial, one approval
  could more than enough)
- If a PR is urgent (hotfix) it can be merged, but it should still be reviewed after-the-fact (reach out to other team members on
  Slack to get their after-the-fact approval)
