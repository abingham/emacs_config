(require 'flycheck)

(setq flycheck-checkers
      '(ada-gnat
        asciidoc
        ;; c/c++-clang
        ;; c/c++-gcc
        ;; c/c++-cppcheck
        cfengine
        chef-foodcritic
        coffee
        coffee-coffeelint
        coq
        css-csslint
        d-dmd
        elixir
        emacs-lisp
        emacs-lisp-checkdoc
        erlang
        eruby-erubis
        fortran-gfortran
        go-gofmt
        go-golint
        go-vet
        go-build
        go-test
        go-errcheck
        haml
        handlebars
        haskell-ghc
        haskell-hlint
        html-tidy
        javascript-jshint
        javascript-eslint
        javascript-gjslint
        json-jsonlint
        less
        lua
        make
        perl
        perl-perlcritic
        php
        php-phpmd
        php-phpcs
        puppet-parser
        puppet-lint
        ;; python-flake8
        ;; python-pylint
        racket
        rpm-rpmlint
        rst
        rst-sphinx
        ruby-rubocop
        ruby-rubylint
        ruby
        ruby-jruby
        rust
        sass
        scala
        scala-scalastyle
        scss
        sh-bash
        sh-posix-dash
        sh-posix-bash
        sh-zsh
        sh-shellcheck
        slim
        tex-chktex
        tex-lacheck
        texinfo
        verilog-verilator
        xml-xmlstarlet
        xml-xmllint
        yaml-jsyaml
        yaml-ruby))
