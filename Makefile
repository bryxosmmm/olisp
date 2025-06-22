exec:
	dune exec parser --ignore-lock-dir && clang ./out/out.ll -lraylib -o ./out/bin/b

execone:
	dune exec parser --ignore-lock-dir && cat ./out/out.ll

compile:
	clang ./out/out.ll -lraylib -o ./out/bin/b -g
