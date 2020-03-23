build: src/Main.hs index.css meetup.json events.json logo.js
	runghc src/Main.hs rebuild

logo.js: src/Logo.elm
	elm make --optimize src/Logo.elm --output logo.js


index.css: src/index.sass
	sassc src/index.sass > index.css


meetup.json:
	curl "https://api.meetup.com/Hannover-Elm-Language-Meetup" | jq . > meetup.json


events.json:
	curl "https://api.meetup.com/Hannover-Elm-Language-Meetup/events" | jq . > events.json


clean:
	rm -f index.css
	rm -f logo.js
	rm -rf _site


distclean: clean
	rm -f events.json
	rm -f meetup.json
	rm -rf _cache
