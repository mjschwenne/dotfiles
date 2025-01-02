{pkgs, ...}: {
  xdg.configFile."superfile/config.toml".text = ''
    # More details are at https://superfile.netlify.app/configure/superfile-config/
    #
    # change your theme
    theme = 'nord'
    #
    # The editor files/directories will be opened with. (leave blank to use the EDITOR environment variable).
    editor = ""
    #
    # Auto check for update
    auto_check_update = true
    #
    # Cd on quit (For more details, please check out https://superfile.netlify.app/configure/superfile-config/#cd_on_quit)
    cd_on_quit = false
    #
    # Whether to open file preview automatically every time superfile is opened.
    default_open_file_preview = true
    #
    # The path of the first file panel when superfile is opened. (DON'T USE '~')
    default_directory = "."
    #
    # Display file sizes using powers of 1000 (kB, MB, GB) instead of powers of 1024 (KiB, MiB, GiB).
    file_size_use_si = false
    #
    # Default sort type (0: Name, 1: Size, 2: Date Modified).
    default_sort_type = 0
    #
    # Default sort order (false: Ascending, true: Descending).
    sort_order_reversed = false
    #
    # Case sensitive sort by name (upper "B" comes before lower "a" if true).
    case_sensitive_sort = false
    #
    # ================   Style =================
    #
    # If you don't have or don't want Nerdfont installed you can turn this off
    nerdfont = true
    #
    # Set transparent background or not (this only work when your terminal background is transparent)
    transparent_background = true
    #
    # File preview width allow '0' (this mean same as file panel),'x' x must be less than 10 and greater than 1 (This means that the width of the file preview will be one xth of the total width.)
    file_preview_width = 0
    #
    # The length of the sidebar. If you don't want to display the sidebar, you can input 0 directly. If you want to display the value, please place it in the range of 3-20.
    sidebar_width = 20
    #
    # Border style
    border_top = '─'
    border_bottom = '─'
    border_left = '│'
    border_right = '│'
    border_top_left = '╭'
    border_top_right = '╮'
    border_bottom_left = '╰'
    border_bottom_right = '╯'
    border_middle_left = '├'
    border_middle_right = '┤'
    #
    # ==========PLUGINS========== #
    #
    # Show more detailed metadata, please install exiftool before enabling this plugin!
    metadata = false
    #
    # Enable MD5 checksum generation for files
    enable_md5_checksum = false
  '';
  xdg.configFile."superfile/hotkeys.toml".text = ''
    # This is maintain by github.com/nonepork
    # I know this is not really that "vim", but the control flow is different.
    # =================================================================================================
    # Global hotkeys (cannot conflict with other hotkeys)
    confirm = ['enter', ''']
    quit = ['ctrl+c', '''] # also know as, theprimeagen troller
    # movement
    list_up = ['k', ''']
    list_down = ['j', ''']
    page_up = ['pgup',''']
    page_down = ['pgdown',''']
    # file panel control
    create_new_file_panel = ['n', ''']
    close_file_panel = ['q', ''']
    next_file_panel = ['tab', ''']
    previous_file_panel = ['shift+tab', ''']
    toggle_file_preview_panel = ['f', ''']
    open_sort_options_menu = ['o', ''']
    toggle_reverse_sort = ['R', ''']
    # change focus
    focus_on_process_bar = ['ctrl+p', ''']
    focus_on_sidebar = ['ctrl+s', ''']
    focus_on_metadata = ['ctrl+m', ''']
    # create file/directory and rename
    file_panel_item_create = ['a', ''']
    file_panel_item_rename = ['r', ''']
    # file operations
    copy_items = ['y', ''']
    cut_items = ['x', ''']
    paste_items = ['p', ''']
    delete_items = ['d', ''']
    # compress and extract
    extract_file = ['ctrl+e', ''']
    compress_file = ['ctrl+a', ''']
    # editor
    open_file_with_editor = ['e', ''']
    open_current_directory_with_editor = ['E', ''']
    # other
    pinned_directory = ['P', ''']
    toggle_dot_file = ['.', ''']
    change_panel_mode = ['m', ''']
    open_help_menu = ['?', ''']
    open_command_line = [':', ''']
    copy_path = ['Y', ''']
    copy_present_working_directory = ['c', ''']
    toggle_footer = ['ctrl+f', ''']
    # =================================================================================================
    # Typing hotkeys (can conflict with all hotkeys)
    confirm_typing = ['enter', ''']
    cancel_typing = ['esc', ''']
    # =================================================================================================
    # Normal mode hotkeys (can conflict with other modes, cannot conflict with global hotkeys)
    parent_directory = ['-', ''']
    search_bar = ['/', ''']
    # =================================================================================================
    # Select mode hotkeys (can conflict with other modes, cannot conflict with global hotkeys)
    file_panel_select_mode_items_select_down = ['J', ''']
    file_panel_select_mode_items_select_up = ['K', ''']
    file_panel_select_all_items = ['A', ''']
  '';

  home.packages = with pkgs; [
    superfile
  ];
}
