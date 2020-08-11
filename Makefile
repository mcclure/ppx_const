build:
	dune build

test:
	dune runtest -f

clean:
	dune clean

.PHONY: build test clean

release:
	@if [ -z "$(VERSION)" ]; then echo "Usage: make release VERSION=1.0.0"; exit 1; fi
	git commit -pm "Prepare for release."
	git tag -a v$(VERSION) -m "Version $(VERSION)"
	git push origin v$(VERSION)

.PHONY: release
