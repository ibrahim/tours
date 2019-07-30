

debug:
	elm make src/Main.elm --output=public/main.js --debug

serve:
	http-server public/ -a localhost -c-1 -p 8000 -g --proxy http://localhost:5555

build: elm-make compress

elm-make:
	elm make src/Main.elm --output=public/main.js --optimize

gschema: 
	elm-graphql http://localhost:5555/graphql --base Tour --output src

compress: uglify gzip

uglify:
	uglifyjs public/main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=public/main.js && uglifyjs public/main.js --mangle --output=public/main.js

gzip:
	gzip -9 public/main.js
