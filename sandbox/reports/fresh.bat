# Clean up directory after compiling

# Files
del Report.aux
del Report.knit.md
del Report.log
del Report.pdf
del Report.Rmd
del Report.tex
del Report.upa
del Report.upb
del Report.utf8.md
del texput.log

# Folders
rmdir /S /Q knitr-figs-pdf
rmdir /S /Q _book
