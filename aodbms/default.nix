{ stdenv, gnu-cobol, zeromq }:
stdenv.mkDerivation {
    name = "aodbms";
    src = ./.;
    buildInputs = [
        gnu-cobol
        zeromq
    ];
}
