build:
	fasm ./out/out.asm && gcc -no-pie -o ./out/bin/b ./out/out.o
