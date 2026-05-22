# Release Process

Notes for publishing `doctor` to opam-repository.
This is a release checklist, not release automation.

Run from a clean checkout. Replace `0.2.0` with the version being released.

## Prepare

Update the version in package metadata, README examples, and `CHANGES.md`.
Check that the source archive URL in `doctor.opam` points at the tag you plan
to publish.

```console
git status --short
opam lint doctor.opam
opam install . --deps-only --with-test
dune build
dune runtest
```

Smoke-test the installed command before tagging:

```console
opam pin add doctor . -y --kind=path
doctor version
doctor check
opam pin remove doctor -y
```

## Tag

Commit the release metadata, then create the tag.
Push the branch and tag after checking the final diff.

```console
git status --short
git tag -a 0.2.0 -m "Release 0.2.0"
```

## Publish

Before the first release, verify current `opam-publish` usage with the installed help
or the current opam documentation. The tool is used to open the opam-repository pull
request for the tagged release.

```console
opam install opam-publish
opam publish --help
```

After the opam-repository PR is merged:

```console
opam update
opam info doctor
opam install doctor
doctor version
doctor check
```

The package metadata currently uses the maintainer's GitHub noreply address. Change
it only when you intentionally want a different public maintainer address on opam.
