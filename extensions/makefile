build-ai:
	ocamlbuild -use-ocamlfind -pkg ppx_deriving_yojson -pkg graphics -pkg unix src/CPbattleship.ml src/CPbattleship.d.byte && \
	rm bin/CPbattleship.d.byte; \
	mkdir bin/_build/; \
	mkdir bin/_build/src; \
	mv _build/src/* bin/_build/src/; \
	mv _build/* bin/_build/; \
	rm -rf _build/; 
	cd bin; \
	ln -s _build/src/CPbattleship.d.byte CPbattleship.d.byte

build-createGame:
	ocamlbuild -use-ocamlfind -pkg ppx_deriving_yojson -pkg graphics -pkg unix src/createGame.ml src/createGame.d.byte; \
	rm createGame.d.byte; \
	mkdir bin/_build/; \
	mkdir bin/_build/src; \
	mv _build/src/* bin/_build/src/; \
	mv _build/* bin/_build/; \
	rm -rf _build/; 
	cd bin; \
	ln -s _build/src/createGame.d.byte createGame.d.byte
build-shootPos:
	ocamlbuild -use-ocamlfind -pkg ppx_deriving_yojson -pkg graphics -pkg unix src/shootPos.ml src/shootPos.d.byte; \
	rm shootPos.d.byte; \
	mkdir bin/_build/; \
	mkdir bin/_build/src; \
	mv _build/src/* bin/_build/src/; \
	mv _build/* bin/_build/; \
	rm -rf _build/; 
	cd bin; \
	ln -s _build/src/shootPos.d.byte shootPos.d.byte
build-getGameData:
	ocamlbuild -use-ocamlfind -pkg ppx_deriving_yojson -pkg graphics -pkg unix src/getGameData.ml src/getGameData.d.byte; \
	rm getGameData.d.byte; \
	mkdir bin/_build/; \
	mkdir bin/_build/src; \
	mv _build/src/* bin/_build/src/; \
	mv _build/* bin/_build/; \
	rm -rf _build/; 
	cd bin; \
	ln -s _build/src/getGameData.d.byte getGameData.d.byte
build-placeShips:
	ocamlbuild -use-ocamlfind -pkg str -pkg ppx_deriving_yojson -pkg graphics -pkg unix src/placeShips.ml src/placeShips.d.byte && \
	rm placeShips.d.byte; \
	mkdir bin/_build/; \
	mkdir bin/_build/src; \
	mv _build/src/* bin/_build/src/; \
	mv _build/* bin/_build/; \
	rm -rf _build/; 
	cd bin; \
	ln -s _build/src/placeShips.d.byte placeShips.d.byte
build:
	make build-createGame; \
	make build-shootPos; \
	make build-getGameData; \
	make build-placeShips

test:
	ocamlbuild -use-ocamlfind -pkg graphics -pkg unix CPbattleship_test.ml CPbattleship_test.d.byte
test-ai:
	ocamlbuild -use-ocamlfind -pkg graphics -pkg unix ai_test.ml ai_test.d.byte
clean:
	ocamlbuild -clean; rm -rf bin/*


#required: yojson ppx_deriving_yojson