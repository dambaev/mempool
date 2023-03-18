{ GIT_COMMIT_HASH}:
args@{config, pkgs, options, lib, ...}:
let
  moduleBackend = import ./module-backend.nix { GIT_COMMIT_HASH = GIT_COMMIT_HASH; };
  moduleFrontend = import ./module-frontend.nix { GIT_COMMIT_HASH = GIT_COMMIT_HASH; };
in
{
  imports = [
    moduleBackend
    moduleFrontend
  ];
}