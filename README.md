# .files

## [home-manager](https://nix-community.github.io/home-manager/index.xhtml#sec-install-standalone)

```sh
nix-channel --add https://github.com/nix-community/home-manager/archive/release-23.11.tar.gz home-manager

nix-channel --update

nix-shell '<home-manager>' -A install

git clone https://github.com/davidpa9708/dotfiles.git

home-manager switch dotfiles
```