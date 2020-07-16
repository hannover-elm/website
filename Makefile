build: json
	cabal build
	cabal run website


watch: json
	ghcid -T ":main -wS"


json: src/meetup.json src/events.json


src/meetup.json:
	curl "https://api.meetup.com/Hannover-Elm-Language-Meetup" | jq . > src/meetup.json


src/events.json:
	curl "https://api.meetup.com/Hannover-Elm-Language-Meetup/events" | jq . > src/events.json


clean:
	rm -f src/events.json
	rm -f src/meetup.json
	rm -rf public


distclean: clean
	rm -rf dist-newstyle elm-stuff
