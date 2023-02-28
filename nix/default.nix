let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/0f8f64b54ed07966b83db2f20c888d5e035012ef.tar.gz";
  pkgs = import nixpkgs {
    config = {};
    overlays = [
      (import ./overlay.nix)
    ];
  };

in {
  op-energy-backend = pkgs.op-energy-backend;
  op-energy-frontend = pkgs.op-energy-frontend;
}
