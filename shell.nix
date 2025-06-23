{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "env";

  buildInputs = with pkgs; [
    ocaml
    dune_3             
    ocamlPackages.findlib
    ocamlPackages.utop 
    ocamlformat         
    ocamlPackages.ocaml-lsp  
    ocamlPackages.menhir  
    ocamlPackages.menhirLib
    ocamlPackages.llvm
    opam
  ];
}

