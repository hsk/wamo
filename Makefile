all:
	ocamlyacc parser.mly
	rm parser.mli
	ocamllex lexer.mll
	ocamlc -w A-8-9-27 -g -o wamo \
		util.ml prolog.ml parser.ml lexer.ml \
		wam.ml wam_emit.ml wam_compiler.ml \
		runtime_mem.ml runtime_trace.ml runtime.ml \
		main.ml
	rm -rf *.cm* parser.ml lexer.ml
update:
	cabal update
tests/wam:
	cd tests && git clone https://github.com/acharal/wam
	patch -u tests/wam/src/Main.hs < tests/patch.txt

wamh: tests/wam
	cd tests/wam && cabal install --only-dependencies && cabal configure && cabal build
	cp tests/wam/dist/build/wam/wam ./wamh
test: wamh
	echo 'diff <(./wamo -i tests/1fact.pl -v) <(./wamh -i tests/1fact.pl -v)' | bash
	echo 'diff <(./wamo -i tests/2multifact.pl -v) <(./wamh -i tests/2multifact.pl -v)' | bash
	echo 'diff <(./wamo -i tests/3and.pl -v) <(./wamh -i tests/3and.pl -v)' | bash
	echo 'diff <(./wamo -i tests/4thread.pl -v) <(./wamh -i tests/4thread.pl -v)' | bash
	echo 'diff <(./wamo -i tests/5unify.pl -v) <(./wamh -i tests/5unify.pl -v)' | bash
	echo 'diff <(./wamo -i tests/6unify.pl -v) <(./wamh -i tests/6unify.pl -v)' | bash
	echo 'diff <(./wamo -i tests/7reverse.pl -v) <(./wamh -i tests/7reverse.pl -v)' | bash
	echo 'diff <(./wamo -i tests/8envprotect.pl -v) <(./wamh -i tests/8envprotect.pl -v)' | bash
	echo 'diff <(./wamo -i tests/9unsafe.pl -v) <(./wamh -i tests/9unsafe.pl -v)' | bash
	echo 'diff <(./wamo -i tests/10unsafe.pl -v) <(./wamh -i tests/10unsafe.pl -v)' | bash
	echo 'diff <(./wamo -i pl/prelude.pl -v) <(./wamh -i pl/prelude.pl -v)' | bash
	echo 'diff <(./wamo -i pl/perm.pl -v) <(./wamh -i pl/perm.pl -v)' | bash
clean:
	rm -rf *.cm* wamo parser.ml lexer.ml wamh
