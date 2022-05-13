# Dotfiles

Hello, this repo holds my configurations for my ubuntu laptops!

![A few screenshots](/Pictures/screenshots/screenshot_collection.png)

Currently I am enjoying the [Catppuccin](https://github.com/catppuccin/catppuccin) theme system-wide.
Here is an overview of the software that I use:

| Software                                                                    | Purpose                  |
| --------------------------------------------------------------------------- | ------------------------ |
| [fish](https://fishshell.com/)                                              | shell                    |
| [kitty](https://github.com/kovidgoyal/kitty)                                | terminal emulator        |
| [qtile](http://www.qtile.org/)                                              | window manager           |
| [picom](https://github.com/yshui/picom)                                     | compositor               |
| [lightdm](https://github.com/canonical/lightdm)                             | display manager          |
| [lightdm-gtk-greeter](https://github.com/Xubuntu/lightdm-gtk-greeter)       | login screen             |
| [neovim](https://neovim.io/)                                                | text editor              |
| [zathura](https://github.com/pwmt/zathura)                                  | PDF viewer               |
| [Thunar](https://docs.xfce.org/xfce/thunar/start)                           | file manager             |
| [ranger](https://github.com/ranger/ranger)                                  | CLI file manager         |
| [dunst](https://github.com/dunst-project/dunst)                             | notification client      |
| [btop](https://github.com/aristocratos/btop)                                | system monitor           |
| [rofi](https://github.com/davatorium/rofi)                                  | application laucher      |
| [papirus](https://github.com/PapirusDevelopmentTeam/papirus-icon-theme)     | icon pack                |
| [papirus-folder](https://github.com/PapirusDevelopmentTeam/papirus-folders) | change folder icon color |
| [Catppuccin-pink](https://github.com/catppuccin/gtk)                        | GTK 3 theme              |
| [flameshot](https://github.com/flameshot-org/flameshot)                     | screenshots              |
| [BetterDiscord](https://betterdiscord.app/)                                 | themed discord client    |
| [spicetify](https://spicetify.app/)                                         | themed spotify client    |
| [JetBrainsMono Nerd Font](https://www.nerdfonts.com/)                       | nerd font                |

## Installation

This section is for how I installed all of this software, focusing on places where extra steps were required
to install and configure the software.

### Cloning this Repository 

Use the below commands to close this repo. This whole idea and process were inspired by 
[this](https://mjones.network/storing-dotfiles-in-a-git-repo) article.
```
git clone --bare git@github.com:mjschwenne/dotfiles.git $HOME/.dotfiles 
git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME checkout
git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME config status.showUntrackedFiles no 
```

### Fish 

For whatever reason, the first command did not create `/usr/local/bin/fish`, which is what the fish docs say
to use, so I set the shell to `/usr/bin/fish` and it worked without a problem.
```
sudo apt install fish 
chsh -s /usr/bin/fish
```
When I was replacing this setup on my second laptop, the colors in fish were wrong, it was not respecting the 
terminal color scheme and I was unsure as to why. Eventually I found that there were hard-coded hex values in 
the `fish_variables` file instead of the color names like 'red' on my other laptop. Removing the file so that 
it was generated again fixed this problem.

### Kitty 

No need to do anything fancy here since once qtile is up and running the terminal keybinding (`mod4 + enter`) 
will launch kitty.
```
sudo apt install kitty
```

### qtile

Follow the documentation for installing from source with `pip`. Note the cairocffi and xcffib stuff since 
this tripped me up. If you install them in the wrong order, you can fix it using 
```
pip install --no-cache-dir --ignore-installed xcffib
pip install --no-cache-dir --ignore-installed cairocffi
```
to force `pip` to install re-install them in the correct order.
```
pip install qtile
```
will install qtile itself, but we have to make a `.desktop` file in xsessions so that lightdm can see qtile 
as an option. Put the below file at `/usr/share/xsessions/qtile.desktop` and `/usr/share/applications/qtile.desktop`
```
[Desktop Entry]
Encoding=UTF-8
Name=Qtile 
Exec=qtile start
Type=Application 
Keywords=wm;tiling
```
In order to get the brightness keys working, make sure that `brightnessctl` is installed and your user is part of the video 
group, which can be done with 
```
sudo usermod -a -G video $LOGNAME
```
Note that you will have to log out and log back in for the change to take effect.

### lightdm 

```
sudo apt install lightdm
```
When prompted, select `lightdm` as the default display manager 

### lightdm-gtk-greeter

```
sudo apt install lightdm-gtk-greeter
```

The default configuration files for `lightdm` and `lightdm-gtk-greeter` are located at `/etc/lightdm/`, however 
I wanted the conf files to be in my home directory which is also this github repo, so I created links for the files.
```
ln /etc/lightdm/lightdm.conf /home/mjs/.config/lightdm/lighdm.conf 
ln /etc/lightdm/lightdm-gtk-greeter.conf /home/mjs/.config/lightdm/lightdm-gtk-greeter.conf 
```
which is probably questionable from a security standpoint... I may change this.

In order for the wallpaper to be correctly shown, copy into `/usr/share/backgrounds/` as lightdm doesn't seem to like 
absolute paths into my home directory for whatever reason. 

### neovim 

Neovim is my favorite text editor and several days of this project was just me learning how to write a neovim config with 
lua.

While it is available as a snap, this project was the first time that I've every run into issues from snaps sandboxing so I'd 
recommend a traditional install of neovim. Also, neovim and the plugins surrounding it move quickly. As such, the package in 
the ubuntu repo is one version out of date and already unsupported by several plugins, so I grabbed the latest `.deb` pre-packaged 
build from their github repository and installed it. 
```
sudo dpkg -i nvim-linux64.deb
```
Are for the rest of the configuration, all that is needed is to pull this repository.

### zathura 

Follow the installation instructions [here](https://github.com/pwmt/zathura#installation).

### thunar 

```
sudo apt install thunar 
```
Now, the above command will install thunar, but it will not set it as the default with `xdg`, so to do that run 
```
xdg-mime default thunar.desktop inode/directory
```
Please be aware that setting thunar as the default application will not change the dialog opened by say a web browser to upload 
or download a file since those applications call the gtk file dialog which is developed to look extremely close to nautilus, 
the GNOME file manager.

### ranger 

```
sudo apt install ranger 
```
I have not had the chance to configure ranger yet and it isn't a top priority at the moment. 

### dunst 

```
sudo apt install dunst 
```
This is an outdated version of dunst which works fine but the config for example requires the deprecated geometry tag. A more 
up to date version would require building it from source, which I may do later.

### btop 

`btop` is a very nice looking system monitor and easy to install on ubuntu 22.04.
```
sudo apt install btop
```
Note that it is not part of the ubuntu 20.04 repository.

### rofi 

I personally prefer rofi over dmenu, but qtile's builtin spawner uses dmenu in the background (I believe), so it is still a 
good idea to install dmenu.
```
sudo apt install rofi 
sudo apt install dmenu 
```

### papirus & papirus-folder

Papirus is a very nice looking icon pack, which since I am using a tiling window manger has a limited and subtle effect since I don't 
see a lot of icons everywhere anymore.
```
sudo add-apt-repository ppa:papirus/papirus 
sudo apt update
sudo apt install papirus-icon-theme
sudo apt install papirus-folders
```
At the moment, I have a lot of pink accent colors so I set the papirus folder icons to pink using 
```
papirus-folders -C pink --theme Papirus-Dark
```

### Catppuccin-pink GTK Theme 

Download the tar ball from the releases page on github and extract it.
They recommand placing it at `~/.themes/Catppuccin-pink` but I wanted it available for the lightdm-gtk-greeter so I put it in 
`/usr/share/themes/Catppuccin-pink`.

### flameshot 

```
sudo apt install flameshot
```

### BetterDiscord 

May sure that discord is installed as a traditional package, then just run the BetterDiscord installer.
The first three packages here are dependencies. 
```
sudo apt install libgconf-2-4 
sudo apt install libappindicator1
sudo apt install libc++1
wget -O discord.deb "https://discordapp.com/api/download?platform=linux&format=deb"
sudo dpkg -i discord.deb
```
Finally, since I am using the catppuccin theme, follow those instructions.

### spicetify 

May sure that spotify is installed as a traditional package, then follow the installation instructions on the 
catppuccin github repo for better safety practices. 
```
sudo chgrp <user> /usr/share/spotify
sudo chgrp <user> -R /usr/share/spotify/Apps
sudo chmod 775 /usr/share/spotify/
sudo chmod 775 -R /usr/share/spotify/Apps
```
Install the catppuccin files in the correct locations (see their installation page), then
```
spicetify config current_theme catppuccin
spicetify config color_scheme pink
spicetify config inject_css 1 replace_colors 1 overwrite_assets 1
spicetify config extensions catppuccin.js
```

### JetBrainsMono Nerd Font 

Download it from the nerd font page, extract and install in `/usr/share/fonts/truetype/jetbrains/` so that lightdm can use it as well.
