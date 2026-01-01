let 
  pkgs = import <nixpkgs> {};
in pkgs.stdenv.mkDerivation {
  name = "gamebooksolver";
  buildInputs = with pkgs; [
    pkg-config
    openssl
    gnum4
    graphviz
    black
  ];
  GIT_SSH_COMMAND="ssh -F /home/simon-marechal/gits/cours/sshconfig";
 }
