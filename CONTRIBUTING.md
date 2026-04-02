## Contributing to the Catppuccin theme for Emacs!

Thanks for taking the time to contribute! This document will go into
detail on how to add support for a new Emacs package in the Catppuccin
theme.

Assuming you have forked the project and have a local clone set up,
adding support for new faces can typically be done with the following
steps:

## Identify the face

Put your point over some text that looks like it's colored
incorrectly, and run M-x `describe-text-properties`. That will open a
Help buffer. Take note of the name displayed next to `face`. This is
what you will customise in the theme.

Feel free to click on the face name and it will display a list of
properties that you can customise in the theme's code. If every
property is unspecified, take a look at the face specified under
`inherit`, which will tell you where this face gets its attributes
from.

## Test out different colors

We can use `set-face-attribute` to test out changes live without
modifying the theme. It takes the following arguments:

```lisp
(set-face-attribute FACE FRAME &rest ARGS)
```

where `FACE` is your chosen face, `FRAME` is nil (meaning all frames
and all future frames) and `&rest ARGS` is pairs of `:attribute value`,
which is described below.

For our purposes, we can use:

```lisp
(set-face-attribute 'YOUR-FACE-NAME nil :attribute (catppuccin-color 'mauve))
```

Replace `'YOUR-FACE-NAME` with the name of your face (keep the
apostrophe!) and replace `:attribute` with the attribute you want
to change. After `:attribute` we have the value `(catppuccin-color 'mauve)`
which will give us the value of the Catppuccin color `mauve` for our
current flavor (of course, feel free to try as many colours as you
like!) You can specify `:attribute color` as many times as you want
to set multiple attributes at once.

Once you've found the ideal color for your face's attribute, you can
add it to the theme.

## Modifying the theme

Open `catppuccin-theme.el` and scroll down until you find patterns
like this:

```lisp
(default :background ,ctp-base :foreground ,ctp-text)
```

These sub-lists follow the format of `(FACE :ATTR VALUE...)`, where
`,ctp-COLOR` can be used to specify the value of the Catppuccin color
`COLOR`.

This list is roughly sorted alphabetically by package name. Find a
place for your package's faces and add them in!

## Testing & committing

To test the theme when I'm making changes I like to add some code to
my init.el temporarily so that I can open a new instance of Emacs and
see the theme running from a clean slate. Use something like this:

```lisp
;; path to your Catppuccin Emacs theme repo
(add-to-list 'load-path "~/git/catppuccin/")
(require 'catppuccin-theme)
(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin :no-confirm)
```

Restart Emacs and make sure every face looks good.

Now commit your changes and make a pull request! (And please
include before & after screenshots for us!)

Commit messages should follow the format of `type: description`.
This is documented in detail over at
[conventionalcommits.org](https://www.conventionalcommits.org/en/v1.0.0),
but a simple commit message for Dired support could be
`feat: dired support` (where `feat` stands for feature).

Thanks!
