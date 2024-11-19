
build:
	cabal update
	cabal build

docker-build:
	docker-compose build

docker-export: docker-build
	docker image save reminderbot-reminderbot > reminderbot.tar

clean:
	cabal clean
	rm reminderbot.tar
