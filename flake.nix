{
  description = "Matt's Personal NixOS Flake";

  nixConfig = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];

    extra-substituters = [
      # Nix community's cache server
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    # There are many ways to reference flake inputs. The most widely used is github:owner/name/reference,
    # which represents the GitHub repository URL + branch/commit-id/tag.

    # Official NixOS package source, using nixos-unstable branch here
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # Master branch, for when packages haven't migrated to the unstable branch
    nixpkgs-master.url = "github:NixOS/nixpkgs";
    # Stable branch, for when packages need to be rolled back
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-25.05";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # secret management
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # waybar
    waybar = {
      url = "github:Alexays/Waybar";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    awww = {
      url = "git+https://codeberg.org/LGFae/awww";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Always up-to-date emacs
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Nvim configuration
    nvf = {
      url = "github:notashelf/nvf";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    spicetify-nix = {
      url = "github:Gerg-L/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    foundry = {
      url = "github:reckenrode/nix-foundryvtt";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware = {
      url = "github:NixOS/nixos-hardware/master";
    };

    wezterm = {
      url = "github:wez/wezterm?dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:nix-community/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    copyparty = {
      url = "github:9001/copyparty";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # `outputs` are all the build result of the flake.
  # A flake can have many use cases and different types of outputs.
  # parameters in `outputs` are defined in `inputs` and can be referenced by their names.
  # However, `self` is an exception, This special parameter points to the `outputs` itself (self-reference)
  # The `@` syntax here is used to alias the attribute set of the inputs's parameter, making it convenient to use inside the function.
  outputs =
    {
      nixpkgs,
      home-manager,
      sops-nix,
      nixos-hardware,
      stylix,
      copyparty,
      ...
    }@inputs:
    let
      system = "x86_64-linux";
      specialArgs = inputs // {
        pkgs-master = import inputs.nixpkgs-master {
          inherit system;
        };
        pkgs-stable = import inputs.nixpkgs-stable {
          inherit system;
        };
      };
      useGlobalPkgs = true;
      useUserPackages = true;
      extraSpecialArgs = inputs // {
        pkgs-master = import inputs.nixpkgs-master {
          inherit system;
        };
        pkgs-stable = import inputs.nixpkgs-stable {
          inherit system;
        };
      };
      overlays = [
        inputs.emacs-overlay.overlay
        copyparty.overlays.default
      ];
    in
    {
      nixosConfigurations = {
        "terra" = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = [
            sops-nix.nixosModules.sops
            stylix.nixosModules.stylix
            ./system/terra.nix

            home-manager.nixosModules.home-manager
            {
              home-manager = {
                inherit useGlobalPkgs useUserPackages extraSpecialArgs;
                users.mjs = import ./home/terra.nix;
              };
              nixpkgs.overlays = overlays;
            }
          ];
        };

        "venus" = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = [
            sops-nix.nixosModules.sops
            stylix.nixosModules.stylix
            ./system/venus.nix

            home-manager.nixosModules.home-manager
            {
              home-manager = {
                inherit useGlobalPkgs useUserPackages extraSpecialArgs;
                users.mjs = import ./home/venus.nix;
              };
              nixpkgs.overlays = overlays;
            }
          ];
        };

        "mars" = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = [
            sops-nix.nixosModules.sops
            stylix.nixosModules.stylix
            copyparty.nixosModules.default
            ./system/mars.nix

            home-manager.nixosModules.home-manager
            {
              home-manager = {
                inherit useGlobalPkgs useUserPackages extraSpecialArgs;
                users.mjs = import ./home/mars.nix;
              };
              nixpkgs.overlays = overlays;
            }
          ];
        };

        "luna" = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = [
            nixos-hardware.nixosModules.microsoft-surface-pro-intel
            sops-nix.nixosModules.sops
            stylix.nixosModules.stylix
            ./system/luna.nix

            home-manager.nixosModules.home-manager
            {
              home-manager = {
                inherit useGlobalPkgs useUserPackages extraSpecialArgs;
                users.mjs = import ./home/luna.nix;
              };
              nixpkgs.overlays = overlays;
            }
          ];
        };

        "sol" = nixpkgs.lib.nixosSystem {
          inherit system specialArgs;
          modules = [
            sops-nix.nixosModules.sops
            stylix.nixosModules.stylix
            ./system/sol.nix

            home-manager.nixosModules.home-manager
            {
              home-manager = {
                inherit useGlobalPkgs useUserPackages extraSpecialArgs;
                users.mjs = import ./home/sol.nix;
              };
              nixpkgs.overlays = overlays;
            }
          ];
        };
      };
    };
}
