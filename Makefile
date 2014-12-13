out.js: React.purs
	psc --main $< -o $@

build.js: out.js
	browserify -d $< -o $@

run: build.js
	node build.js

