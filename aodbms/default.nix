{ stdenv, gnu-cobol }:
stdenv.mkDerivation {
    name = "escad";
    src = ./.;
    buildInputs = [
        gnu-cobol
    ];
}
