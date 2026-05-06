# Release Process

Notes for publishing `doctor` to opam-repository.

Run these from a clean checkout. Replace `0.1.0` with the version being
released.

## Prepare

Update the version in the package metadata, README examples, and changelog.
Confirm that the source archive URL in `doctor.opam` points at the tag you plan
to publish.

```console
git status --short
opam lint doctor.opam
opam install . --deps-only --with-test
opam exec -- dune build
opam exec -- dune runtest
```

Check the installed command before tagging:

```console
opam pin add doctor . -y --kind=path
doctor check
doctor version
opam pin remove doctor -y
```

## Tag

```console
git status --short
git add dune-project doctor.opam README.md CHANGES.md RELEASE.md
git commit -m "Prepare 0.1.0 release"
git tag -a 0.1.0 -m "Release 0.1.0"
git push origin main
git push origin 0.1.0
```

## Publish

```console
opam install opam-publish
opam publish
```

After the opam-repository PR is merged:

```console
opam update
opam info doctor
opam install doctor
doctor version
doctor check
```

The package metadata currently uses the maintainer's GitHub noreply address.
Change it only when you intentionally want a different public maintainer address
on opam.
