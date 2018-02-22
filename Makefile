default:
	stack build
	stack exec -- bridge
	wget --quiet https://nodejs.org/dist/v8.9.4/node-v8.9.4-linux-x64.tar.xz
	tar xf node-v8.9.4-linux-x64.tar.xz
	export PATH="/app/node-v8.9.4-linux-x64/bin:${PATH}" && cd site && \
		npm install && \
		npm run psc -- update && \
		npm run pulp -- build --to app.js && \
		npm run uglify -- app.js --output app.min.js --compress --mangle && \
		mkdir -p ../.local/bin/ && \
		cp app.min.js ../.local/bin/

install:
	stack build --copy-bins
