
build:
	elm make src/Main.elm --output=public/main.js

gschema: 
	elm-graphql http://localhost:5555/graphql --base Tour --output src
