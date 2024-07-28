-- See https://wezfurlong.org/wezterm/

local wezterm = require 'wezterm';

local wezterm = require 'wezterm'
return {
  front_end = "WebGpu",
  webgpu_preferred_adapter = {
  backend = "Vulkan",
  device = 26880,
  device_type = "DiscreteGpu",
  driver = "radv",
  driver_info = "Mesa 24.0.5",
  name = "AMD Radeon R5 M465 Series (RADV ICELAND)",
  vendor = 4098,
},

  font = wezterm.font("JetBrainsMono Nerd Font", {weight = "Light"}),
  font_size = 11.0,
  color_scheme = "Catppuccin Mocha",
  window_background_opacity = 1.0,
  hide_tab_bar_if_only_one_tab = true,
  default_prog = { "fish" },
  enable_wayland = true,
}

