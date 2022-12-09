{ pkgs ? import <nixpkgs> {} }:
let haskellWithDeps = pkgs.ghc.withPackages(pkgs: with pkgs; [
	stylish-haskell
	hindent
	QuickCheck
	haskell-language-server
	split
]);
in pkgs.mkShell {
	nativeBuildInputs = with pkgs; [
		haskellWithDeps
		gnumake
		hyperfine
	];
}
