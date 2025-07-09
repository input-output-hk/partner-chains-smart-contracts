# Developer guidelines

## Updating the changelog
We use [Changie](https://changie.dev/) to manage our changelog. It is included in our nix develop shell.
When getting ready to create a PR, create a new changelog entry by running `changie new`.
Make sure to commit the created changelog fragment along with your changes.

## When creating a GitHub release
- Necessary versions are updated using `./release.sh`
- Push a git tag with the version (e.g. `v3.0.0`)
