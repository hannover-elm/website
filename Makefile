build: src/Main.hs index.css meetup.json events.json
	runghc src/Main.hs rebuild


index.css: src/index.sass
	sassc src/index.sass > index.css


meetup.json:
	curl "https://api.meetup.com/Hannover-Elm-Language-Meetup" | jq . > meetup.json


events.json:
	curl "https://api.meetup.com/Hannover-Elm-Language-Meetup/events" | jq . > events.json


clean:
	rm -f index.css
	rm -f events.json
	rm -f meetup.json


distclean: clean
	rm -rf _site _cache
