OSTree commands 

```
sudo ostree remote add wezterm-nightly https://download.copr.fedorainfracloud.org/results/atim/wezterm-nightly/fedora-40-x86_64
sudo ostree remote add starship https://copr.fedorainfracloud.org/coprs/atim/starship/repo/fedora-40/atim-starship-fedora-40.repo
rpm-ostree install https://github.com/twpayne/chezmoi/releases/download/v2.51.0/chezmoi-2.51.0-x86_64.rpm
rpm-ostree install fish wezterm distrobox starship fastfetch
rpm-ostree rebase ostree-image-signed:docker://ghcr.io/ublue-os/sericea-<main,surface,nvidia>:latest
< REBOOT HERE >
rpm-ostree rebase ostree-unverified-registry:ghcr.io/ublue-os/sericea-<main,surface,nvidia>:latest
```
