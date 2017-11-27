all:
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	ocamlc -w A-8-9-27 -g -o wamo \
		util.ml syntax.ml parser.ml lexer.ml \
		ir.ml compiler.ml emit.ml \
		memory.ml trace.ml vm.ml \
		main.ml
	rm -rf *.cm* parser.ml lexer.ml
mkdata:
	cd tests && make clean && make
test:
	./wamo -i tests/1fact.pl -v | diff tests/1fact.wam -
	./wamo -i tests/2multifact.pl -v | diff tests/2multifact.wam -
	./wamo -i tests/3and.pl -v | diff tests/3and.wam -
	./wamo -i tests/4thread.pl -v | diff tests/4thread.wam -
	./wamo -i tests/5unify.pl -v | diff tests/5unify.wam -
	./wamo -i tests/6unify.pl -v | diff tests/6unify.wam -
	./wamo -i tests/7reverse.pl -v | diff tests/7reverse.wam -
	./wamo -i tests/8envprotect.pl -v | diff tests/8envprotect.wam -
	./wamo -i tests/9unsafe.pl -v | diff tests/9unsafe.wam -
	./wamo -i tests/10unsafe.pl -v | diff tests/10unsafe.wam -
clean:
	rm -rf *.cm* wamo parser.ml lexer.ml wamh
	cd tests && make clean

# reference haskell implementation
update:
	cabal update
tests/wam:
	cd tests && git clone https://github.com/acharal/wam
	patch -u tests/wam/src/Main.hs < tests/patch.txt
wamh: tests/wam
	cd tests/wam && cabal install --only-dependencies && cabal configure && cabal build
	cp tests/wam/dist/build/wam/wam ./wamh
test2: wamh
	./wamh -i tests/1fact.pl -v | diff tests/1fact.wam -
	./wamh -i tests/2multifact.pl -v | diff tests/2multifact.wam -
	./wamh -i tests/3and.pl -v | diff tests/3and.wam -
	./wamh -i tests/4thread.pl -v | diff tests/4thread.wam -
	./wamh -i tests/5unify.pl -v | diff tests/5unify.wam -
	./wamh -i tests/6unify.pl -v | diff tests/6unify.wam -
	./wamh -i tests/7reverse.pl -v | diff tests/7reverse.wam -
	./wamh -i tests/8envprotect.pl -v | diff tests/8envprotect.wam -
	./wamh -i tests/9unsafe.pl -v | diff tests/9unsafe.wam -
	./wamh -i tests/10unsafe.pl -v | diff tests/10unsafe.wam -
