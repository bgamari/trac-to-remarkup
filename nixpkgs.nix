let src = fetchTarball {
  url = "https://github.com/NixOS/nixpkgs-channels/archive/749a3a0d00b5d4cb3f039ea53e7d5efc23c296a2.tar.gz";
  sha256 = "14dqndpxa4b3d3xnzwknjda21mm3p0zmk8jbljv66viqj5plvgdw";
};
in import src
