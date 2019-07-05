set makeprg=cabal
nnoremap <Leader>m :make v2-build -f development<CR>
nnoremap <Leader>t :make v2-test -f development<CR>
set path=.,src/**/

iabbr >< ×
iabbr o ∘
iabbr <= ⊆

syn keyword hsVarSym ⊆ 
au! BufWritePost docs/*.{md,yaml} silent !pandoc --from markdown --standalone %:r.md %:r.yaml --out %:r.html 2> %:r.log
