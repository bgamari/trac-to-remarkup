let 
  commit = "18ef1da03d4042ec68a607731041fd8d8bf7176b";
  src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${commit}.tar.gz";
    sha256 = "00jv1k9mkh9kz5bmi7jf7737512ks0kzsaizzmgqvpp1z6ayxs21";
  };
in import src
