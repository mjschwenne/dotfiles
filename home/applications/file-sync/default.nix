{ osConfig, lib, ... }:

let
  enable = lib.lists.any (x: x == osConfig.networking.hostName) [
    "terra"
    "venus"
  ];
  filterList = ''
    - *venv*/**
    - .mypy_cache/**
    - __pycache__/**
    - .git/**
    - .ipynb_checkpoints/**
    - .jupyter/**
    - .quarto/**
    - node_modules/**
    - ltximg/**
    - *.org#
    - **/.auc*
    - *.aux
    - *.lof
    - *.log
    - *.lot
    - *.fls
    - *.out
    - *.toc
    - *.fmt
    - *.fot
    - *.cb
    - *.cb2tex-auto/
    - .direnv/**
    - .envrc
  '';
  workDirs = [
    {
      name = "agenda";
      localPath = "$HOME/Documents/agenda";
      remote = "copyparty:agenda";
      interval = "15m";
      mode = "timer";
    }
    {
      name = "archive";
      localPath = "$HOME/Documents/archive";
      remote = "copyparty:archive";
      interval = "12h";
      mode = "timer";
    }
    {
      name = "classes";
      localPath = "$HOME/Documents/classes";
      remote = "copyparty:classes";
      interval = "30m";
      mode = "realtime";
    }
    {
      name = "kdb";
      localPath = "$HOME/kdb";
      remote = "copyparty:kdb";
      interval = "4h";
      mode = "realtime";
    }
    {
      name = "projects";
      localPath = "$HOME/Documents/projects";
      remote = "copyparty:projects";
      interval = "30m";
      mode = "timer";
    }
    {
      name = "supernote";
      localPath = "$HOME/Documents/supernote";
      remote = "copyparty:supernote";
      interval = "1h";
      mode = "timer";
    }
    {
      name = "zotero";
      localPath = "$HOME/Zotero/storage";
      remote = "copyparty:zotero";
      interval = "1h";
      mode = "timer";
    }
  ];
  personalDirs = [
    {
      name = "personal";
      localPath = "$HOME/Documents/personal";
      remote = "copyparty:personal";
      interval = "6h";
      mode = "timer";
    }
    {
      name = "ttrpg";
      localPath = "$HOME/Documents/ttrpg";
      remote = "copyparty:ttrpg";
      interval = "12h";
      mode = "timer";
    }
  ];
in
{
  imports = [ ./rclone-sync.nix ];

  services.rclone-sync = {
    inherit enable filterList;

    syncDirs =
      workDirs
      ++ (if lib.lists.any (x: x == osConfig.networking.hostName) [ "terra" ] then personalDirs else [ ]);
  };
}
