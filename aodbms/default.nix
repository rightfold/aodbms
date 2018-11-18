{ stdenv, gnu-cobol, zeromq }:
stdenv.mkDerivation {
    name = "escad";
    src = ./.;
    buildInputs = [
        gnu-cobol
        zeromq
    ];
}
