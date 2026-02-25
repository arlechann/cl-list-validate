.PHONY: test clean

test:
	XDG_CACHE_HOME=/tmp sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:test-system :list-validate)'

clean:
	find ./ -type f -name '*.fasl' | xargs -n1 rm -f
