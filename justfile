test:
  #!/usr/bin/env sh
  for testfile in *.test.el; do
    echo emacs --script $testfile && emacs --script $testfile;
    exit=$?
    if [ $exit -ne 0 ]; then exit $exit; fi
  done

run: test
  emacs --script catppuccin-theme.el --eval "(load-theme 'catppuccin t)"
