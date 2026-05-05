# Release Process

This document is for maintainers preparing a `doctor` release for opam-repository.

## Checklist

```console
# 1. Confirm release metadata.
git status
opam lint

# 2. Build and test.
opam install . --deps-only --with-test
dune build
dune runtest

# 3. Test local install.
opam pin add doctor . -y
doctor check
doctor version
opam remove doctor

# 4. Commit.
git status
git add .
git commit -m "Prepare 0.1.0 release"

# 5. Tag.
git tag -a 0.1.0 -m "Release 0.1.0"
git push origin main
git push origin 0.1.0

# 6. Publish.
opam install opam-publish
opam publish

# 7. After the opam-repository PR is merged.
opam update
opam install doctor
doctor check
doctor version
```

For the `0.1.0` release, the maintainer email is the GitHub noreply address
used in the package metadata. Update it only if you intentionally want a
different public maintainer address in opam.
