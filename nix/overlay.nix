self: super:
let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/0f8f64b54ed07966b83db2f20c888d5e035012ef.tar.gz";
  pkgs = import nixpkgs {
    config = {};
    overlays = [ ];
  };
  args = {
    pkgs = pkgs;
  };
in{
  op-energy-backend = (pkgs.callPackage ./derivation.nix args).op-energy-backend;
  op-energy-frontend = (pkgs.callPackage ./derivation.nix args).op-energy-frontend;
}
