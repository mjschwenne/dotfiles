{ pkgs, ... }:
{
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    profiles.default = {
      enableUpdateCheck = false;
      enableExtensionUpdateCheck = false;
      extensions = with pkgs.vscode-extensions; [
        vscodevim.vim
        anthropic.claude-code
        mkhl.direnv
        leanprover.lean4
        # lean4 deps
        tamasfe.even-better-toml
      ];
      userSettings = {
        "vim.easymotion" = true;
        "vim.useSystemClipboard" = true;
        "vim.hlsearch" = true;
        "vim.highlightedyank.enable" = true;
        "[nix]"."editor.tabSize" = 2;
        "workbench.startupEditor" = "none";
        "claudeCode.preferredLocation" = "panel";
      };
    };
  };
}
