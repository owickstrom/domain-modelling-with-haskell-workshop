index.html: workshop/workshop.rst Makefile workshop/workshop.js.html
	pandoc \
		-s \
		-V subtitle='<a href="https://bit.ly/2HP4SCX">bit.ly/2HP4SCX</a>' \
		--include-after workshop/workshop.js.html \
		--include-after workshop/footer.html \
		--css workshop/style.css \
		--css workshop/iosevka/iosevka.css \
		--css 'https://fonts.googleapis.com/css?family=Source+Sans+Pro' \
		--highlight-style monochrome \
		--toc \
		--mathjax \
		--base-header-level=2 \
		-o index.html \
		workshop/workshop.rst
