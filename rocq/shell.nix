with import <nixpkgs> {};
mkShell {
  nativeBuildInputs = [
    ocaml
    opam
    pkg-config
    gtk3
    gtk3-x11
    gcc
    bintools-unwrapped
    gmp
    adwaita-icon-theme
  ];
  shellHook = ''
    test -r '/home/simon-marechal/.opam/opam-init/init.sh' && . '/home/simon-marechal/.opam/opam-init/init.sh' > /dev/null 2> /dev/null || true
  '';
}
