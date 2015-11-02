call plug#begin('~/.config/nvim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'kien/ctrlp.vim'
Plug 'rking/ag.vim'

call plug#end()

set number
let mapleader = "\<Space>"

colorscheme molokai

" Tab Settings
set tabstop=4
set softtabstop=4
set shiftwidth=4
set noexpandtab   " use tabs, not spaces

set list!
"set listchars=tab:▸\ ,eol:¬
set listchars=trail:·,precedes:«,extends:»,tab:▸\ ,eol:¬
set vb t_vb= " Turn off visual bell, error flash
