{ osConfig, lib, ... }:

let
  enable = lib.lists.any (x: x == osConfig.networking.hostName) [
    "terra"
    "venus"
    "luna"
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
    - .zotero-*
    - *.{db,db-shm,db-wal}
  '';
  workDirs = [
    {
      name = "agenda";
      localPath = "$HOME/Documents/agenda";
      remote = "copyparty:agenda";
      calendar = "*:0/30";
      mode = "realtime";
    }
    {
      name = "archive";
      localPath = "$HOME/Documents/archive";
      remote = "copyparty:archive";
      calendar = "0/8:00";
      mode = "timer";
    }
    {
      name = "classes";
      localPath = "$HOME/Documents/classes";
      remote = "copyparty:classes";
      calendar = "*:0/30";
      mode = "realtime";
    }
    {
      name = "kdb";
      localPath = "$HOME/kdb";
      remote = "copyparty:kdb";
      calendar = "0/4:00";
      mode = "realtime";
    }
    {
      name = "projects";
      localPath = "$HOME/Documents/projects";
      remote = "copyparty:projects";
      calendar = "*:0/30";
      mode = "timer";
    }
    {
      name = "supernote";
      localPath = "$HOME/Documents/supernote";
      remote = "copyparty:supernote";
      calendar = "0/8:00";
      mode = "timer";
    }
    {
      name = "zotero";
      localPath = "$HOME/Zotero/storage";
      remote = "copyparty:zotero";
      calendar = "0/1:00";
      mode = "timer";
    }
  ];
  personalDirs = [
    {
      name = "personal";
      localPath = "$HOME/Documents/personal";
      remote = "copyparty:personal";
      calendar = "0/6:00";
      mode = "timer";
    }
    {
      name = "ttrpg";
      localPath = "$HOME/Documents/ttrpg";
      remote = "copyparty:ttrpg";
      calendar = "0/12:00";
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
