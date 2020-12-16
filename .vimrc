" open
autocmd vimenter * if !argc() | Explore | endif
autocmd BufWinEnter,WinEnter term://* startinsert

filetype plugin indent on    " required

let g:python3_host_prog = '/usr/bin/python3'

set tabstop=2
set shiftwidth=2
set expandtab
set runtimepath+=$HOME/.vim/plugin
" set paste
set mouse=a
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
set guioptions-=m  "remove menu bar
set guioptions+=LlRrb
set guioptions-=TLlRrb
set guicursor=
set linespace=1
set backspace=indent,eol,start


let $NVIM_TUI_ENABLE_CURSOR_SHAPE=0

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

" Remap leader
let mapleader = ","

" Key Mappings
map <F2> :Vexplore<CR>
map <C-J> <C-W>j<C-W>_
map <C-K> <C-W>k<C-W>_
map <C-l> :tabnext<CR>
map <C-h> :tabprev<CR>
map <C-t> :tabnew<CR>
map <A-l> :wincmd l<CR>
map <A-h> :wincmd h<CR>
map <C-Tab> :bnext<cr>
map <C-d> :VimwikiToggleListItem<cr>
map <C-S-Tab> :bprevious<cr>

" vim fugitive stuff
map <F3> :Gstatus<CR>
map <F4> :Gcommit<CR>
map <F6> :Gdiff<CR>

map <Leader><Space> :YcmCompleter GoToDefinition<CR>
noremap <Leader>b :b#<CR>
nnoremap <Leader>a :Ag<CR>
nnoremap <Leader>cf :let @+=expand("%")<CR>
" absolute path (/something/src/foo.txt)
nnoremap <Leader>cF :let @+=expand("%:p")<CR>
" filename (foo.txt)
nnoremap <Leader>ct :let @+=expand("%:t")<CR>
" directory name (/something/src)
nnoremap <Leader>ch :let @+=expand("%:p:h")<CR>
noremap <Leader>l :Log<CR>
inoremap <F8> :Log<CR>
noremap <Leader>q :cclose<CR>
noremap <Leader>j :%!python3 -m json.tool<CR>

" Insert current date
" :inoremap <F7> <C-R>=strftime("%a %d %b %Y")<CR>
:inoremap <F7> <C-R>=strftime("# %a %Y-%m-%d")<CR>


set nocompatible              " be iMproved, required
filetype off                  " required



call plug#begin('~/.vim/plugged')
Plug 'amix/vim-zenroom2'
Plug 'mhinz/vim-signify'
Plug 'davidhalter/jedi-vim'
Plug 'ekalinin/Dockerfile.vim'
Plug 'honza/vim-snippets'
Plug 'hynek/vim-python-pep8-indent'
Plug 'junegunn/limelight.vim'
Plug 'luochen1990/rainbow'
Plug 'mattn/webapi-vim'
Plug 'majutsushi/tagbar'
Plug 'mattn/gist-vim'
Plug 'mileszs/ack.vim'
Plug 'mklabs/split-term.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'SirVer/ultisnips'
" Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
Plug 'tpope/vim-classpath'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-vinegar'
Plug 'tommcdo/vim-fubitive'
Plug 'tommcdo/vim-exchange'
Plug 'shumphrey/fugitive-gitlab.vim'
" Plug 'Valloric/YouCompleteMe'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vimwiki/vimwiki'
Plug 'godlygeek/tabular'
Plug 'gavocanov/vim-js-indent'
Plug 'w0rp/ale'

Plug 'LucHermitte/lh-vim-lib'
Plug 'LucHermitte/local_vimrc'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Colors
Plug 'jonathanfilip/vim-lucius'
Plug 'whatyouhide/vim-gotham'
Plug 'cseelus/vim-colors-lucid'
Plug 'cnj4/horseradish256'
Plug 'morhetz/gruvbox'

" Syntax
Plug 'othree/yajs.vim'
Plug 'ap/vim-css-color'
Plug 'rust-lang/rust.vim'
Plug 'posva/vim-vue'
Plug 'elixir-editors/vim-elixir'
Plug 'leafgarland/typescript-vim'
Plug 'digitaltoad/vim-pug'
Plug 'plasticboy/vim-markdown'
Plug 'mustache/vim-mustache-handlebars'
Plug 'mxw/vim-jsx'
Plug 'pangloss/vim-javascript'
Plug 'reedes/vim-colors-pencil'
Plug 'junegunn/gv.vim'


" All of your Plugs must be added before the following line
call plug#end()            " required

autocmd FileType git commit DiffGitCached | wincmd p

" Colourscheme

if empty($LIGHT)
  colorscheme lucius
  LuciusDarkHighContrast
  let g:gitgutter_override_sign_column_highlight = 0
  let g:gitgutter_sign_removed = '-'
  highlight ColorColumn ctermfg=1 ctermbg=235
  highlight TabLineSel ctermfg=2 ctermbg=235
  highlight StatusLine ctermfg=2 ctermbg=235
  highlight GitGutterAdd guibg=#DDFBE6 ctermfg=41 cterm=bold
  highlight GitGutterChange guibg=#FCD876 ctermfg=58 cterm=bold
  highlight GitGutterChangeDelete guibg=#FCD876 ctermfg=52 cterm=bold
  highlight GitGutterDelete guibg=#FAC5CD ctermfg=52 cterm=bold
else
  colorscheme lucius
  LuciusWhiteHighContrast
  highlight ColorColumn ctermfg=1 ctermbg=224 cterm=NONE guifg=#FF2222 guibg=#FFDFDF gui=bold
  highlight StatusLine ctermbg=1
  let g:rainbow_conf = {
  \  'ctermfgs': ['darkblue', 'darkyellow', 'darkcyan', 'darkmagenta']
  \}
  highlight GitGutterAdd guibg=#DDFBE6 ctermfg=28 cterm=bold
  highlight GitGutterChange guibg=#FCD876 ctermfg=172 cterm=bold
  highlight GitGutterChangeDelete guibg=#FCD876 ctermfg=172 cterm=bold
  highlight GitGutterDelete guibg=#FAC5CD ctermfg=196 cterm=bold
endif

" let g:gruvbox_italic=1
" colorscheme gruvbox
" set background=dark

" Color customisations
highlight WildMenu ctermbg=172


set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.
highlight ExtraWhitespace ctermbg=red guibg=#d75f00
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" Spelling
highlight SpellBad term=underline gui=undercurl guisp=Orange

" Fancy coloured parenthese
" au VimEnter * RainbowParenthesesToggleAll
" au Syntax * RainbowParenthesesLoadRound
" au Syntax * RainbowParenthesesLoadBraces
"
let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle

" Custom tabstops
au FileType ruby setl sw=2 sts=2 ts=2 et
au FileType javascript setl sw=2 sts=2 ts=2 et
au FileType typescript setl sw=2 sts=2 ts=2 et
au FileType php setl sw=2 sts=2 ts=2 et
au FileType html setl sw=2 sts=2 ts=2 et
au FileType htmldjango setl sw=2 sts=2 ts=2 et
au FileType yaml setl sw=2 sts=2 ts=2 et
au FileType pug setl sw=2 sts=2 ts=2 et
au FileType css setl sw=2 sts=2 ts=2 et
au FileType scss setl sw=2 sts=2 ts=2 et
au FileType less setl sw=2 sts=2 ts=2 et
au FileType markdown,vimwiki setl sw=2 sts=2 ts=2 et

au BufRead,BufNewFile *.markdown set filetype=markdown
au BufRead,BufNewFile *.md       set filetype=markdown

" Jump to sudo
cmap w!! w !sudo tee % >/dev/null
vmap <Enter> :Eval<CR>

" Airline status bar stuff
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_section_y = 0
let g:airline_section_x = 0
let g:airline_left_sep = '▶'
let g:airline_right_sep = ''
let g:airline_powerline_fonts = 1
let g:airline_symbols.branch = '⎇'
let g:airline_theme='base16_ashes'
let g:airline_detect_paste=0
let g:airline_mode_map = {
    \ '__' : '-',
    \ 'n'  : 'N',
    \ 'i'  : 'I',
    \ 'R'  : 'R',
    \ 'c'  : 'C',
    \ 'v'  : 'V',
    \ 'V'  : 'V',
    \ '' : 'V',
    \ 's'  : 'S',
    \ 'S'  : 'S',
    \ '' : 'S',
    \ }


" Vim gist stuff
let g:gist_clip_command = 'xclip -selection clipboard'
let g:gist_detect_filetype = 1
let g:gist_post_private = 1
let g:gist_open_browser_after_post = 1

" YCM
let g:ycm_goto_buffer_command='vertical-split'
let b:ycm_largefile=1
let g:ycm_server_keep_logfiles=1
set completeopt-=preview

" Vim Handlebars
let g:mustache_abbreviations = 1

" Set font
set guifont=Inconsolata\ 12

" UltiSnips
let g:UltiSnipsExpandTrigger="<C-f>"
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetsDir="~/.vim/plugged/vim-snippets/snippets"

" Vim Markdown
let g:vim_markdown_folding_level = 6

set foldmethod=indent
set foldlevel=20
set foldlevelstart=20

" Vim-JSX
let g:jsx_ext_required = 0

let g:vimwiki_list = [{
          \ 'custom_wiki2html': 'wiki2html',
          \ 'path': '~/vimwiki/',
          \ 'syntax': 'markdown',
          \ 'ext': '.md',
          \ 'template_path': '~/vimwiki/templates/',
          \ 'template_default': 'def_template',
          \ 'template_ext': '.html'}]


" NeoVim stuff:
augroup AutoSwap
        autocmd!
        autocmd SwapExists *  call AS_HandleSwapfile(expand('<afile>:p'), v:swapname)
augroup END

function! AS_HandleSwapfile (filename, swapname)
        " if swapfile is older than file itself, just get rid of it
        if getftime(v:swapname) < getftime(a:filename)
                call delete(v:swapname)
                let v:swapchoice = 'e'
        endif
endfunction
autocmd CursorHold,BufWritePost,BufReadPost,BufLeave *
  \ if isdirectory(expand("<amatch>:h")) | let &swapfile = &modified | endif

augroup checktime
    au!
    if !has("gui_running")
        "silent! necessary otherwise throws errors when using command
        "line window.
        autocmd BufEnter,CursorHold,CursorHoldI,CursorMoved,CursorMovedI,FocusGained,BufEnter,FocusLost,WinLeave * checktime
    endif
augroup END

autocmd BufNewFile,BufRead *.test.js.snap set syntax=javascript
autocmd BufNewFile,BufRead *.tsx set filetype=typescript.tsx

" Typescript settings
hi tsxTagName guifg=#E06C75
hi tsxCloseString guifg=#F99575
hi tsxCloseTag guifg=#F99575
hi tsxAttributeBraces guifg=#F99575
hi tsxEqual guifg=#F99575
hi tsxAttrib guifg=#F8BD7F cterm=italic

"
" Auto-repl based on file
"
let g:term_repls = {
\   'python': 'python',
\   'javascript': 'node',
\   'ruby': 'irb'
\ }

function! RunRepl ()
    let l:cmd = '80VTerm'
    let l:paste = ''

    for linenum in range(a:firstline, a:lastline)
        let l:paste = l:paste . getline(linenum)
    endfor

    execute 'yank'
    if has_key(g:term_repls, &filetype)
        execute l:cmd g:term_repls[&filetype]
    else
        execute l:cmd
    endif
endfunction

command! -range Repl :call RunRepl()


" Run ripgrep on selection
"
function! s:get_visual_selection()
    " Why is this not a built-in Vim script function?!
    let [line_start, column_start] = getpos("'<")[1:2]
    let [line_end, column_end] = getpos("'>")[1:2]
    let lines = getline(line_start, line_end)
    if len(lines) == 0
        return ''
    endif
    let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
    let lines[0] = lines[0][column_start - 1:]
    return join(lines, "\n")
endfunction

function! RunRg ()
    let l:cmd = "Rg " . s:get_visual_selection()
    echom l:cmd
    execute l:cmd
endfunction

command! -range RG :call RunRg()
vnoremap <Leader>g :RG<CR>

"
" Vimwiki daily diary
"
function! RunLog ()
    execute '80vsplit'
    call vimwiki#diary#goto_diary_index(0)
endfunction

command! -range Log :call RunLog()

noremap <C-p> :Files<CR>

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --word-regexp --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif
let $FZF_DEFAULT_COMMAND = 'ag -g ""'


let g:ale_python_black_executable = '/home/stuart/venvs/black/bin/black'
let g:ale_elixir_mix_executable = 'docker exec -t minipd_web_1 mix'

let g:ale_fixers = {
  \ '*': ['remove_trailing_lines', 'trim_whitespace'],
  \ 'javascript': ['prettier_eslint', 'prettier'],
  \ 'vue': ['prettier'],
  \ 'jsx': ['prettier_eslint', 'prettier'],
  \ 'elixir': ['mix_format'],
  \ }
" \ 'python': ['black'],

let g:ale_linters = {
  \ 'python': ['mypy', 'flake8'],
  \ 'elixir': ['credo'],
  \ }

let g:ale_lint_delay = 1000
let g:ale_sign_error = '⤫'
let g:ale_sign_warning = '⚠'
let g:ale_fix_on_save = 1
let g:airline#extensions#ale#enabled = 1
set autoread

nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

autocmd BufEnter term://* startinsert
autocmd BufLeave term://* stopinsert

set guioptions-=m
set guioptions-=t
set guioptions-=T
set guioptions-=r
set guioptions-=L

" Neovim specific
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=0
