index.html: workshop/workshop.rst Makefile workshop/workshop.js.html
	pandoc \
		-s \
		--include-after workshop/workshop.js.html \
		--include-after workshop/footer.html \
		--css workshop/style.css \
		--css workshop/iosevka/iosevka.css \
		--css 'https://fonts.googleapis.com/css?family=Source+Sans+Pro' \
		--highlight-style haddock \
		--toc \
		--mathml \
		--base-header-level=2 \
		-o index.html \
		workshop/workshop.rst
