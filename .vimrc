" open NERDTree if no files
autocmd vimenter * if !argc() | NERDTree | endif

set tabstop=4
set shiftwidth=4
set expandtab
set runtimepath+=$HOME/.vim/plugin
set paste
set laststatus=2
set ff=unix
set hlsearch
set incsearch
set t_Co=256
set encoding=utf-8

" give me auto indenting even for files vi does not recognize
set ai
set smartindent

" Make vim fill the screen height
set guiheadroom=0
set guioptions-=m  "remove menu bar
set guioptions+=LlRrb
set guioptions-=TLlRrb
set linespace=1
set backspace=indent,eol,start

" Keep vim stuff out of the way
set bdir=/home/stuart/.vimtmp
set dir=/home/stuart/.vimtmp
set undodir=/home/stuart/.vimtmp
set undofile

" Required for vimwiki plugin
set nocompatible
filetype plugin on

let &colorcolumn="80"
set cursorcolumn
syn on

" Key Mappings
map <C-J> <C-W>j<C-W>_
map <C-K> <C-W>k<C-W>_
map <C-l> :tabnext<CR>
map <C-h> :tabprev<CR>
map <C-t> :tabnew<CR>
map <C-Tab> :bnext<cr>
map <C-d> :VimwikiToggleListItem<cr>
map <C-Backspace> :bprevious<cr>
map <C-Return> :YcmCompleter GoTo<cr>

map <F2> :NERDTreeToggle<CR>
map <F3> :Gstatus<CR>
map <F4> :Gcommit<CR>
map <F6> :Gdiff<CR>
map <F7> :YcmCompleter GetDoc<cr>
map <F8> :Goyo<CR>
map <F9> :VimwikiTabMakeDiaryNote<cr>

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'

Plugin 'amix/vim-zenroom2'
Plugin 'airblade/vim-gitgutter'
Plugin 'ap/vim-css-color'
Plugin 'davidhalter/jedi-vim'
Plugin 'ekalinin/Dockerfile.vim'
Plugin 'guns/vim-clojure-static'
Plugin 'hynek/vim-python-pep8-indent'
Plugin 'joonty/vdebug.git'
Plugin 'junegunn/goyo.vim'
Plugin 'junegunn/limelight.vim'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'mattn/webapi-vim'
Plugin 'majutsushi/tagbar'
Plugin 'mattn/gist-vim'
Plugin 'mileszs/ack.vim'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'pangloss/vim-javascript'
Plugin 'rking/ag.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/syntastic'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-classpath'
Plugin 'tpope/vim-fireplace'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-markdown'
Plugin 'tpope/vim-projectionist'
Plugin 'tommcdo/vim-exchange'
Plugin 'Valloric/YouCompleteMe'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'Xuyuanp/nerdtree-git-plugin'


" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

autocmd FileType git commit DiffGitCached | wincmd p
" Vimwiki templates
"          \ 'syntax': 'markdown',
let g:vimwiki_list = [{'path': '~/vimwiki/',
          \ 'ext': '.md',
          \ 'template_path': '~/vimwiki/templates/',
          \ 'template_default': 'def_template',
          \ 'template_ext': '.html'}]

let g:vim_markdown_folding_disabled=1

let g:NERDTreeDirArrows=1

" Colourscheme
colorscheme lucius
LuciusLightHighContrast
highlight CursorColumn guibg=#DEDEDE
highlight ColorColumn ctermfg=1 ctermbg=7 cterm=NONE guifg=#FF2222 guibg=#FFDFDF gui=bold

set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.

" Pink Comments
" hi Comment guifg=#F06EFF gui=bold

" Spelling
if has("gui_running")
    highlight SpellBad term=underline gui=undercurl guisp=Orange
endif

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" Fancy coloured parenthese
au VimEnter * RainbowParenthesesToggleAll
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadBraces

" Custom tabstops
au FileType ruby setl sw=2 sts=2 ts=2 et
au FileType javascript setl sw=4 sts=4 ts=4 et
au FileType php setl sw=2 sts=2 ts=2 et
au FileType html setl sw=4 sts=4 ts=4 et

au BufNewFile,BufRead *.cs set filetype=coffee
au BufRead,BufNewFile *.markdown set filetype=mkd
au BufRead,BufNewFile *.md       set filetype=mkd

" change the mapleader from \ to ,
let mapleader=","

" Insert current date
:inoremap <F7> <C-R>=strftime("%a %d %b %Y")<CR>

" ZenMode
nnoremap <silent> <leader>z :Goyo<cr>

" replace currently selected text with default register
" without yanking it
vnoremap <leader>p "_dP

autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS

" Prevent Jedi doc string
autocmd FileType python setlocal completeopt-=preview

" Jump to sudo
cmap w!! w !sudo tee % >/dev/null
vmap <Enter> :Eval<CR>

" Airline status bar stuff
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_left_sep = '▶'
let g:airline_right_sep = '◀'
let g:airline_symbols.branch = '⎇'
let g:airline_theme='solarized'

" Vim gist stuff
let g:gist_clip_command = 'xclip -selection clipboard'
let g:gist_detect_filetype = 1
let g:gist_post_private = 1
let g:gist_open_browser_after_post = 1

" Syntastic checkers
let g:syntastic_python_checkers = ['flake8']
let g:syntastic_javascript_checkers = ['eslint']

" Use Ag if available
if executable('ag')
  " Use Ag instead of grep
  set grepprg=ag\ --nogroup\ --nocolor
  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g "" --ignore-dir=.git/'
endif

" Set font
set guifont=Inconsolata\ 9

"" Limelight Goyo integration
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

" Projectionist config
let g:projectionist_heuristics = {
\   "Adfuser_UAT_instructions.md": {
\     "aam_adfuser/static/js/views/*.js": {
\       "alternate": "aam_adfuser/static/html/{}.handlebars",
\       "type": "views"
\     },
\     "aam_adfuser/database_upgrade/versions/*_downgrade.sql": {
\       "alternate": "aam_adfuser/database_upgrade/versions/{}_upgrade.sql",
\       "type": "migration"
\     },
\     "aam_adfuser/database_upgrade/versions/*_upgrade.sql": {
\       "alternate": "aam_adfuser/database_upgrade/versions/{}_downgrade.sql",
\       "type": "migration"
\     },
\     "app/models/*.js": {
\       "alternate": "app/serializers/{}.js",
\       "type": "model"
\     },
\     "app/components/*/component.js": {
\       "alternate": "app/components/{}/template.hbs",
\       "type": "component"
\     },
\     "app/components/*/template.hbs": {
\       "alternate": "app/components/{}/styles.scss",
\       "type": "template"
\     }
\   }
\}
