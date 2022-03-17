NOTE: if you need table of contents in github, hit a button "=" in the left upper corner of the region, where this file is being displayed.

# Brief

This document describes deployment and development procedure of Op-energy toy exchange.

Currently, we only provide support for deployment to DigitalOcean using terraform.

There is a manual installation procedure, described at https://github.com/dambaev/mempool-overlay/blob/services-based-build/README.md, which walks you step-by-step in a procedure of getting NixOS on DigitalOcean and deploying op-energy toy exchange.

## DO Deployment preparations

Scripts expect to use DigitalOcean API token to perform a deploy. If you don't have any:
1 navigate to https://cloud.digitalocean.com/account/api/tokens;
2 hit 'Generate New Token';
3 enter token's name, ensure, that 'write' access is selected and hit 'Generate Token';
4 copy-patse token into some file so it can be reused later for scripts. In this tutorial, this file will be located at<repo>/nix/DO_TOKEN. This path is excluded from git tracking, so you can keep it there as well.

## Development deployment to DigitalOcean

Development deployment of op-energy toy exchange is supposed to consist of those steps:

1 fork the repo, checkout branch op-energy-master and create new branch with your new feature:

```
git clone https://github.com/<your account here>/op-energy
git checkout -b op-energy-new-feature
```

2 commit and push your changes, navigate to <repo>/nix/development and issue:

2.1 install Terraform (on NixOS):

```
nix-shell ../shell.nix
```

2.2 install Terraform (no your OS);

3 perform deploy with script `deploy_dev.sh`. This scripts requires at least DO_TOKEN and any of SSH_KEYS_BY_NAMES or SSH_KEYS_IDS to be set:

```
DO_TOKEN=$(cat ../DO_TOKEN) SSH_KEYS_BY_NAMES="user1@domain1 user2@domaine2" ./deploy_dev.sh
```

In this example, new droplet will allow access to user root for SSH keys "user1@domain1" and "user2@domain2". By default, script will add droplet with name "op-energy-dev" in the "nyc1" DO region. You can run `./deploy_dev.sh` script without any variables set to get the usage reference with other variables that it can be used with.

This script will deploy new small droplet and 2 volumes for signet network. As there will be build process running during deployment, script will take sometime. It will report progress.
At the end, script will show you an IP address of the new droplet to which you will be able to connect either with SSH or with browser.

4 ssh into new droplet and switch op-energy repo to your branch:

```
ssh -A root@<dropletIP>
cd /etc/nixos
git pull && git submodule update --remote
git checkout -b op-energy-new-feature # set parent repo into a new branch, which will not be pushed into repo and exist only to keep op-energy overlay with non-standard branch
git submodule set-branch -b op-energy-new-feature overlays/op-energy # now switch to new branch
git submodule update --remote
git commit overlays/op-energy -m "overlays/op-energy: switch to op-energy-new-feature"
```

5 rebuild config with your new branch:

```
nixos-rebuild switch
```

6 test your changes by navigating with your browser to http://dropletIP/signet

7 when you will be ready, create pull request to merge your changes into op-energy-master branch. After the merge, production instances will switch to the new version.

Development instances will perform periodical fetching of configurations from git repo and will try to apply them. So `nixos-rebuild switch` happen automatically within 10 minutes period.

## Production deployment

In order to get a production setup of op-energy toy exchange, you need to:

1 clone the repo, checkout branch op-energy-master 

```
git clone --branch op-energy-master https://github.com/dambaev/op-energy.git
```

2.1 install Terraform (on NixOS):

```
nix-shell ../shell.nix
```

2.2 install Terraform (no your OS);

3 navigate to op-energy/nix/production and perform deploy with script `deploy.sh`. This scripts requires at least DO_TOKEN and any of SSH_KEYS_BY_NAMES or SSH_KEYS_IDS to be set:

```
DO_TOKEN=$(cat ../DO_TOKEN) SSH_KEYS_BY_NAMES="user1@domain1 user2@domaine2" ./deploy.sh
```

In this example, new droplet will allow access to user root for SSH keys "user1@domain1" and "user2@domain2". By default, script will add droplet with name "op-energy" in the "nyc1" DO region. You can run `./deploy.sh` script without any variables set to get the usage reference with other variables that it can be used with.

This script will deploy new 4 vCPU and 8GB of RAM droplet and 2 volumes for mainnet network. As there will be build process running during deployment, script will take sometime. It will report progress.
At the end, script will show you an IP address of the new droplet to which you will be able to connect either with SSH or with browser.

After the initial deployment, bitcoind and electrs will need time to fetch blockchain and compute it's index. 

Production instance will fetch updates from op-energy-master branch periodically and will switch to new commits if there will be any.

You can force update with:

```
ssh -A root@dropletIP
systemctl start nixos-apply & journal -f | grep -E "(nixos-apply|nixos-upgrade|op-energy-backend-build|op-energy-frontend-build)"
```

## Deployment to a local NixOS instance

Deployment to a local NixOS instance can be useful because it can be faster than deployment to cloud, but still fully functional.
For this, you will need to:

1 navigate to /etc/nixos, create `overlays` directory:

```
cd /etc/nixos
mkdir overlays
cd overlays
```

2 clone repo https://gthub.com/dambaev/op-energy-development

```
git clone --recursive https://github.com/dambaev/op-energy-development
cd ..
```

3 generate secrets for local instances:

```
nix-shell overlays/op-energy-development/overlays/op-energy/nix/shell.nix
./overlays/op-energy-development/overlays/op-energy/nix/gen-psk.sh /etc/nixos/private/ signet
exit
```

4 edit `/etc/nixos/configuration.nix` to import `./overlays/op-energy-development/local-build-container.nix`. To do that, you need to add this file into `imports` list like that:

```
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./overlays/op-energy-development/local-build-container.nix # this line should be added
    ];
```

5 rebuild the config:

```
nixos-rebuild switch
```

6 start container with op-energy instance:

```
systemctl start container@build
```

7 now you can navigate your browser to `http://192.168.100.17/` to get access to the frontend

8 in order to update your instance, you need to run:

```
cd /etc/nixos/overlays/op-energy-developoment
git submodule update --remote
nixos-rebuild switch
```

or, you can change the branch if you want to test some feature before merging:

```
cd /etc/nixos/overlays/op-energy-developoment
git checkout master
git checkout -b op-energy-some-feature-branch
git submodule set-branch -b op-energy-some-feature-branch
git submodule update --remote
git commit -a -m op-energy-some-feature-branch
nixos-rebuild switch
```

## Frontend development. The fast flow

General development flow can be considered as slow because it should provide reproducible environment.
If you want to have fast development flow, you may use the following steps. Notice, that this method can't guarantee packagesto be pinned.

1 fork the repo, checkout branch op-energy-master and create new branch with your new feature:

```
git clone https://github.com/<your account here>/op-energy
cd op-energy
git checkout -b op-energy-new-feature
```

2 ensure that you have `npm` installed. In case if you are running NixOS or have `nix` package manager installed, you can use:

```
nix-shell nix/shell.nix
```

3 build and run development web server:

```
cd frontend
npm install
npm run build
npm run start
```

4 start ssh session into some existing instance of op-energy. In this example, I will use development instance running signet backend:

```
ssh root@<dropletIP> -L8889:127.0.0.1:8995 "while true; do sleep 10s; echo ping; done"
```

5 open browser and navigate to "http://localhost:4200"

6 now you can edit source code of the frontend and it will be reloaded as soon as you will save any change.

7 push your changes and create pull request by navigating to github -> pull requests -> create pull request. Choose 'base' repo 'op-energy-master' and you current repo as 'compare' one.

8 as soon as pull request will be merged, production instance will rebuild and deploy new version within 10 minutes.

## Backend development
