let g:hdevtools_options="-g-isrc -g-idist/build -g-no-user-package-conf -g-package-conf.hsenv/ghc_pkg_db --socket=/tmp/hdevtools-" . join(split($PWD,"\/"),"-") . "\.sock"
