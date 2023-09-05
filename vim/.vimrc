set number
syntax on
filetype plugin indent on
set hlsearch
set incsearch
set shiftwidth=2
set tabstop=2
set expandtab
set autoindent
set hidden
let mapleader = "\<Space>"
set backspace=indent,eol,start
set history=200
set wildmenu
set ttimeout
set ttimeoutlen=100
set display=truncate
set sidescroll=5
set nowrap
set iskeyword+=-

" When there are tabs in a file, you cannot see where they are.  To make them visible:
set list
set listchars=tab:>-,trail:-

" Handles moving around wraps in insert and normal mode
set whichwrap=b,s,<,>,[,]

nnoremap <Leader><Leader> <c-^>
nnoremap <Leader>b <esc>:buffers<cr>:buffer
nnoremap <Leader>d <esc>:bdelete<cr>
nnoremap <Leader>e <esc>:edit<Space>
nnoremap <Leader>m <esc>:make<cr>
nnoremap <Leader>q <esc>:quit<cr>
nnoremap <Leader>w <esc>:write<cr>
nnoremap <c-n> <esc>:NERDTreeToggle<cr>

" enable automatic running of :RustFmt when you save a buffer.
let g:rustfmt_autosave = 1

" set runtimepath^=~/.vim/bundle/ctrlp.vim
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

set t_Co=256   " This is may or may not needed.

set background=dark
colorscheme PaperColor
