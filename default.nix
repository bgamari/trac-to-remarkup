{ nixpkgs ? import ./nixpkgs.nix {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, bytestring, casing
      , connection, containers, data-default, directory, exceptions
      , filepath, http-client-tls, http-types, megaparsec
      , mtl, NoTrace, postgresql-simple, pretty-show, process, servant
      , servant-client, silently, split, stdenv, stm, temporary, text
      , time, transformers, taggy, lifted-base
      }:
      mkDerivation {
        pname = "trac-to-remarkup";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring casing containers megaparsec mtl NoTrace
          postgresql-simple process servant servant-client split temporary
          text time taggy lifted-base
        ];
        executableHaskellDepends = [
          async base bytestring connection containers data-default directory
          exceptions filepath http-client-tls http-types
          megaparsec mtl postgresql-simple servant-client stm text time
          transformers
        ];
        testHaskellDepends = [
          base directory filepath pretty-show silently
        ];
        license = stdenv.lib.licenses.bsd3;
        doHaddock = false; # Haddock inexplicably breaks
      };


  withDwarf = haskellPackages: haskellPackages.override { 
    overrides = self: super: {
      mkDerivation = xs: super.mkDerivation (xs // 
        { configureFlags = (xs.configureFlags or "") + "--ghc-options=-g --disable-executable-stripping --disable-library-stripping"; }
      );
      #hspec = self.callHackage "hspec" "2.5.9" {};
      #hspec-core = self.callHackage "hspec-core" "2.5.9" {};
      #hspec-discover = self.callHackage "hspec-discover" "2.5.9" {};
      #tasty = self.callHackage "tasty" "1.2" {};
    };
  };
  haskellPackages = if compiler == "default"
                       then withDwarf pkgs.haskellPackages
                       else withDwarf pkgs.haskell.packages.${compiler};

  drv = with nixpkgs.haskell.lib; haskellPackages.callPackage f {
    NoTrace = doJailbreak haskellPackages.NoTrace;
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
