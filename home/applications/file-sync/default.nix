_:

let
  filterList = ''
    - **/*.{db,db-wal,db-shm}
    - **/.nextcloudsync.log
    - **/*venv*/
    - **/.mypy_cache/
    - **/__pycache__/
    - **/.ipynb_checkpoints/
    - **/.jupyter/
    - **/.quarto/
    - **/node_modules/
    - **/ltximg/
    - **/.auctex-auto/
    - **/.direnv/
    - **/.envrc
  '';
in
{
  imports = [ ./rclone-sync.nix ];

  services.rclone-sync = {
    enable = false;
    inherit filterList;

    syncDirs = [
      {
        name = "test";
        localPath = "$HOME/Downloads/test";
        remote = "nextcloud:test";
        interval = "1m";
        mode = "timer";
      }
    ];
  };
}
