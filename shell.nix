{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:

let
  mysmtml = pkgs.ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "smtml";
    version = "0.13.0";

  src = pkgs.fetchgit {
    url = "https://github.com/formalsec/smtml";
    branchName = "filipe/issue-471";
    rev = "8f6a6bea75560de7a4b0b7de3713513969a66590";
    hash = "sha256-8B8vXz/QGeQ4xnX5tcHiTrWjEY5jd0SI1lY+SyOZx9E=";
  };

  nativeBuildInputs = with pkgs.ocamlPackages; [
    menhir
  ];

  propagatedBuildInputs = with pkgs.ocamlPackages; [
    bos
    cmdliner
    dolmen_model
    dolmen_type
    dune-build-info
    fpath
    hc
    menhirLib
    mtime
    ocaml_intrinsics
    patricia-tree
    prelude
    scfg
    yojson
    z3
    zarith
  ];
  });
in

pkgs.mkShell {
  name = "owi-dev-shell";
  dontDetectOcamlConflicts = true;
  nativeBuildInputs = with pkgs.ocamlPackages; [
    dune_3
    findlib
    bisect_ppx
    pkgs.framac
    pkgs.mdbook
    pkgs.mdbook-plugins
    mdx
    menhir
    merlin
    ocaml
    ocamlformat
    ocp-browser
    ocp-index
    ocb
    odoc
    sedlex
    # unwrapped because wrapped tries to enforce a target and the build script wants to do its own thing
    pkgs.llvmPackages.clang-unwrapped
    # lld + llc isn't included in unwrapped, so we pull it in here
    pkgs.llvmPackages.bintools-unwrapped
    pkgs.rustc
    pkgs.zig
    pkgs.makeWrapper
  ];
  buildInputs = with pkgs.ocamlPackages; [
    bos
    cmdliner
    crowbar
    digestif
    dolmen_type
    dune-build-info
    dune-site
    hc
    integers
    menhirLib
    ocamlgraph
    ocaml_intrinsics
    patricia-tree
    prelude
    processor
    scfg
    mysmtml
    synchronizer
    uutf
    xmlm
    yojson
    z3
    zarith
  ];
  shellHook = ''
    export PATH=$PATH:${pkgs.lib.makeBinPath [
      pkgs.llvmPackages.bintools-unwrapped
      pkgs.llvmPackages.clang-unwrapped
      pkgs.rustc
      pkgs.zig
    ]}
  '';
}
