{
  description = "Matt's Personal NixOS Flake";

  nixConfig = {
    experimental-features = ["nix-command" "flakes"];

    extra-substituters = [
      # Nix community's cache server
      "https://nix-community.cachix.org"
      "https://hyprland.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
    ];
  };

  # This is the standard format for flake.nix. `inputs` are the dependencies of the flake,
  # and `outputs` function will return all the build results of the flake.
  # Each item in `inputs` will be passed as a parameter to the `outputs` function after being pulled and built.
  inputs = {
    # There are many ways to reference flake inputs. The most widely used is github:owner/name/reference,
    # which represents the GitHub repository URL + branch/commit-id/tag.

    # Official NixOS package source, using nixos-unstable branch here
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # Master branch, for when packages haven't migrated to the unstable branch
    nixpkgs-master.url = "github:NixOS/nixpkgs";

    # home-manager, used for managing user configuration
    home-manager = {
      url = "github:nix-community/home-manager";
      # The `follows` keyword in inputs is used for inheritance.
      # Here, `inputs.nixpkgs` of home-manager is kept consistent with the `inputs.nixpkgs` of the current flake,
      # to avoid problems caused by different versions of nixpkgs.
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/Hyprland";

    sddm-sugar-candy-nix = {
      url = "gitlab:Zhaith-Izaliel/sddm-sugar-candy-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    spicetify-nix.url = "github:the-argus/spicetify-nix";

    foundry.url = "github:reckenrode/nix-foundryvtt/f624c0ceabe13dd876ecff871e0dc7f55f96e993";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    eww-tray.url = "github:ralismark/eww/tray-3";

    llama.url = "github:ggerganov/llama.cpp";
  };

  # `outputs` are all the build result of the flake.
  # A flake can have many use cases and different types of outputs.
  # parameters in `outputs` are defined in `inputs` and can be referenced by their names.
  # However, `self` is an exception, This special parameter points to the `outputs` itself (self-reference)
  # The `@` syntax here is used to alias the attribute set of the inputs's parameter, making it convenient to use inside the function.
  outputs = {
    nixpkgs,
    home-manager,
    ...
  } @ inputs: {
    nixosConfigurations = {
      # By default, NixOS will try to refer the nixosConfiguration with its hostname.
      # so the system named `nixos-test` will use this configuration.
      # However, the configuration name can also be specified using `sudo nixos-rebuild switch --flake /path/to/flakes/directory#<name>`.
      # The `nixpkgs.lib.nixosSystem` function is used to build this configuration, the following attribute set is its parameter.
      # Run `sudo nixos-rebuild switch --flake .#nixos-test` in the flake's directory to deploy this configuration on any NixOS system
      "terra" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";

        # The Nix module system can modularize configuration, improving the maintainability of configuration.
        #
        # Each parameter in the `modules` is a Nix Module, and there is a partial introduction to it in the nixpkgs manual:
        #    <https://nixos.org/manual/nixpkgs/unstable/#module-system-introduction>
        # It is said to be partial because the documentation is not complete, only some simple introductions
        #    (such is the current state of Nix documentation...)
        # A Nix Module can be an attribute set, or a function that returns an attribute set.
        # If a Module is a function, this function can only have the following parameters:
        #
        #  lib:     the nixpkgs function library, which provides many useful functions for operating Nix expressions
        #            https://nixos.org/manual/nixpkgs/stable/#id-1.4
        #  config:  all config options of the current flake
        #  options: all options defined in all NixOS Modules in the current flake
        #  pkgs:   a collection of all packages defined in nixpkgs.
        #           you can assume its default value is `nixpkgs.legacyPackages."${system}"` for now.
        #           can be customed by `nixpkgs.pkgs` option
        #  modulesPath: the default path of nixpkgs's builtin modules folder,
        #               used to import some extra modules from nixpkgs.
        #               this parameter is rarely used, you can ignore it for now.
        #
        # Only these parameters can be passed by default.
        # If you need to pass other parameters, you must use `specialArgs` by uncomment the following line

        specialArgs =
          inputs
          // {
            pkgs-master = import inputs.nixpkgs-master {
              system = system;
            };
          };
        modules = [
          # Import the configuration.nix we used before, so that the old configuration file can still take effect.
          # Note: /etc/nixos/configuration.nix itself is also a Nix Module, so you can import it directly here
          inputs.sddm-sugar-candy-nix.nixosModules.default
          ./system/terra.nix

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.mjs = import ./home/terra.nix;
            home-manager.extraSpecialArgs =
              inputs
              // {
                pkgs-master = import inputs.nixpkgs-master {
                  system = system;
                };
              };

            # The second one is a function which constructs the overlay, apparently
            nixpkgs.overlays = [inputs.emacs-overlay.overlay (inputs.llama.overlays.default)];
          }
        ];
      };

      "mars" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";

        specialArgs =
          inputs
          // {
            pkgs-master = import inputs.nixpkgs-master {
              system = system;
            };
          };
        modules = [
          inputs.sddm-sugar-candy-nix.nixosModules.default
          ./system/mars.nix

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.mjs = import ./home/mars.nix;
            home-manager.extraSpecialArgs =
              inputs
              // {
                pkgs-master = import inputs.nixpkgs-master {
                  system = system;
                };
              };

            nixpkgs.overlays = [inputs.emacs-overlay.overlay];
          }
        ];
      };

      "luna" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";

        specialArgs =
          inputs
          // {
            pkgs-master = import inputs.nixpkgs-master {
              system = system;
            };
          };
        modules = [
          inputs.nixos-hardware.nixosModules.microsoft-surface-pro-intel
          inputs.sddm-sugar-candy-nix.nixosModules.default
          ./system/luna.nix

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.mjs = import ./home/luna.nix;
            home-manager.extraSpecialArgs =
              inputs
              // {
                pkgs-master = import inputs.nixpkgs-master {
                  system = system;
                };
              };

            nixpkgs.overlays = [inputs.emacs-overlay.overlay];
          }
        ];
      };

      "sol" = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";

        specialArgs =
          inputs
          // {
            pkgs-master = import inputs.nixpkgs-master {
              system = system;
            };
          };
        modules = [
          ./system/sol.nix

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.mjs = import ./home/sol.nix;
            home-manager.extraSpecialArgs =
              inputs
              // {
                pkgs-master = import inputs.nixpkgs-master {
                  system = system;
                };
              };
          }
        ];
      };
    };
  };
}
