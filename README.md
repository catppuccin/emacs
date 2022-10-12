<h3 align="center">
<img src="https://raw.githubusercontent.com/catppuccin/catppuccin/main/assets/logos/exports/1544x1544_circle.png" width="100" alt="Logo"/><br/>
<img src="https://raw.githubusercontent.com/catppuccin/catppuccin/main/assets/misc/transparent.png" height="30" width="0px"/>
  Catppuccin for <a href="https://www.gnu.org/software/emacs/">Emacs</a>
<img src="https://raw.githubusercontent.com/catppuccin/catppuccin/main/assets/misc/transparent.png" height="30" width="0px"/>
</h3>

<p align="center">
<a href="https://github.com/catppuccin/emacs/stargazers"><img src="https://img.shields.io/github/stars/catppuccin/emacs?colorA=363a4f&colorB=b7bdf8&style=for-the-badge"></a>
<a href="https://github.com/catppuccin/emacs/issues"><img src="https://img.shields.io/github/issues/catppuccin/emacs?colorA=363a4f&colorB=f5a97f&style=for-the-badge"></a>
<a href="https://github.com/catppuccin/emacs/contributors"><img src="https://img.shields.io/github/contributors/catppuccin/emacs?colorA=363a4f&colorB=a6da95&style=for-the-badge"></a>
</p>

<p align="center">
<img src="assets/Screenshot.webp"/>
</p>

## Previews

<details>
<summary>🌻 Latte</summary>
<img src="assets/Latte.webp"/>
</details>
<details>
<summary>🪴 Frappé</summary>
<img src="assets/Frappe.webp"/>
</details>
<details>
<summary>🌺 Macchiato</summary>
<img src="assets/Macchiato.webp"/>
</details>
<details>
<summary>🌿 Mocha</summary>
<img src="assets/Mocha.webp"/>
</details>

## Installation

1. Download repo locally.

2. Add your theme of choice to:
   `.emacs.d/themes` if you use vanilla emacs or `.doom.d/themes` if you use Doom emacs <i>(If the `themes` folder doesn't exist make a folder with its name)</i>
3. Install autothemer using either package.el or your choice of package manager to install from MELPA.

## Using
In your `.emacs.d` or `.doom.d` add the following lines in `config.el`
```
(use-package autothemer
  :ensure t)

(load-theme 'catppuccin-(mocha, macchiato, frappe or latte go here) t)
```

## 💝 Thanks to

- [pspiagicw](https://github.com/pspiagicw)
- [samuelnihbos](https://github.com/samuelnihbos)
- [konrad1977](https://github.com/konrad1977)
- [Name](https://github.com/NamesCode)

&nbsp;
<p align="center"><img src="https://raw.githubusercontent.com/catppuccin/catppuccin/main/assets/footers/gray0_ctp_on_line.svg?sanitize=true" /></p>
<p align="center">Copyright &copy; 2021-present <a href="https://github.com/catppuccin" target="_blank">Catppuccin Org</a>
<p align="center"><a href="https://github.com/catppuccin/catppuccin/blob/main/LICENSE"><img src="https://img.shields.io/static/v1.svg?style=for-the-badge&label=License&message=MIT&logoColor=d9e0ee&colorA=363a4f&colorB=b7bdf8"/></a></p>
