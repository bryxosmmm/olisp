build:
	dune exec parser && clang ./out/out.ll -o ./out/bin/b

compile:
	clang ./out/out.ll -o ./out/bin/b
