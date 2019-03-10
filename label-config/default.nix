{ stdenv, wrapPython, python-gitlab, pyyaml }:

stdenv.mkDerivation {
  name = "label-config";
  src = ./.;
  nativeBuildInputs = [ wrapPython ];
  pythonPath = [ pyyaml python-gitlab ];
  installPhase = ''
    install -D -t $out/bin dump-labels.py
    install -D -t $out/bin push-labels.py
  '';
  postFixup = ''
    wrapPythonPrograms
  '';
}

