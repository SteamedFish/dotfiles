set list
" set listchars=tab:→\ ,eol:↩︎,trail:·,extends:⤻,precedes:⤺
set listchars=tab:→\ ,trail:·,extends:⤻,precedes:⤺
set spelllang=en,cjk
set fileencodings=ucs-bom,utf-8,gbk,cp936,default,latin1
set wrap

" only MacVim GUI has transparency
if has("gui_macvim")
    set macligatures
    set transparency=15
endif

" better diff auglrithm
if has("patch-8.1.0360")
    set diffopt+=internal,algorithm:patience
endif
