{ stdenv, wrapPython, python-gitlab }:

stdenv.mkDerivation {
  name = "label-config";
  src = ./.;
  nativeBuildInputs = [ wrapPython ];
  pythonPath = [ python-gitlab ];
  installPath = ''
    install -D -t $out/bin dump-labels.py
    install -D -t $out/bin push-labels.py
  '';
  postFixup = ''
    wrapPythonPrograms
  '';
}

