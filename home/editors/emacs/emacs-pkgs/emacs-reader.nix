{
  melpaBuild,
  fetchFromGitea,
  gcc,
  gnumake,
  pkg-config,
  mupdf-headless,
}:
melpaBuild rec {
  ename = "reader";
  pname = "emacs-reader";
  version = "20261014";
  src = fetchFromGitea {
    domain = "codeberg.org";
    owner = "divyaranjan";
    repo = pname;
    rev = "b8c886346b9fea5492a21f6d94e8830fcea7359e";
    sha256 = "0fk79x1r9vkjzb68a7ks08d8ra50dgdgxz04msqjca7ijygzvqa0";
  };
  files = ''(:defaults "render-core.so")'';
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [
    gcc
    mupdf-headless
    gnumake
    pkg-config
  ];
  # Only build the dynamic module.  Upstream's `all' target also generates
  # reader-autoloads.el, and `:defaults' would then ship it in the package
  # tarball.  package.el only inserts its "(add-to-list 'load-path ...)"
  # prologue when it *creates* the autoloads file, so a pre-existing one
  # leaves the package directory off load-path and byte-compiling
  # reader-bookmark.el fails with "Cannot open load file ... reader".
  preBuild = "make render-core.so";
}
