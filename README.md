# mjschwenne's dotfiles 

Welcome!

I recently moved from nixos to fedora sericea, the sway atomic distribution.
It wasn't an easy choice to move away from nixos. 
I really like the idea of a fully declarative operating system and I think that nixos does a good job with this, *however*, updates where becoming a hassle and I never took a systematic approach to learning the `nix` language. 
Relying on the internet to provide the magic `nix` incantations can only take to so far and it never imparts actual learning. 
This caused me to royally struggle to setup flakes for project development and even though flakes are supposed to be reproducible, I was having issues with copying a flake to a sibling directory and the environment no longer working. 
When that happened, my will to power through all of the recent nixos issues broke and I began to look for alternatives. 
I know that I wanted to have an immutable distribution and that's when I started looking at Fedora Silverblue. 
I was intrigued by the container based setup, which seemed to promise the parts of a flake that I actually used, isolated development environments which can be at least somewhat managed through containerfiles. 
Of course, I prefer tiling window managers and was already running sway on my computers, so naturally sericea was the move for me. 

I actually didn't switch to fedora right away. I played around with it in a virtual machine and decided to install it on my laptop that I'm not really using over the summer but I encountered some issues getting background processes to run in containers. 
But other than that, everything seemed to be working. 
I installed and configured `distrobox` on my `nix` machines and was happily running my development environments in containers anyways.
After about a week of on-and-off tinkering, I decided to move back to nixos but stop using flakes for development environments and rely solely on containers. 
It seemed like the perfect combination for me...
And while I was reinstalling nixos on my laptop I decided to run a system update on my desktop. 
The update actually when smoother than the last several, but the next morning `sway` wouldn't launch, locked up my computer and produced no error messages. 
This gave me the resolve to try fedora atomic again and this time in about a half hour I was able to run the background processes I needed in a container, so I decided to fully pull the plug on nixos. 
Now I have fedora on my laptop and desktop, and will be moving my surface and server laptop over as time allows (but it should happen before the semester starts). 
Unfortunately, I lost all of the work that I didn't bother to back up when I reinstalled nixos on my laptop, but I way able to get back all of that progress very quickly.

## OSTree commands 

Here is a record of the setup command for `ostree` to install a few programs I want on the host machine.

```
sudo wget -P /etc/yum.repos.d/ https://copr.fedorainfracloud.org/coprs/atim/starship/repo/fedora-40/atim-starship-fedora-40.repo
sudo wget -P /etc/yum.repos.d/ https://copr.fedorainfracloud.org/coprs/wezfurlong/wezterm-nightly/repo/fedora-40/wezfurlong-wezterm-nightly-fedora-40.repo
rpm-ostree install https://github.com/twpayne/chezmoi/releases/download/v2.51.0/chezmoi-2.51.0-x86_64.rpm
rpm-ostree install fish wezterm distrobox starship fastfetch
rpm-ostree rebase ostree-unverified-registry:ghcr.io/ublue-os/sericea-<main,surface,nvidia>:latest
systemctl reboot
rpm-ostree rebase ostree-image-signed:docker://ghcr.io/ublue-os/sericea-<main,surface,nvidia>:latest
```

Then to apply these dotfiles, just run 

```
chezmoi init --apply mjschwenne
```
