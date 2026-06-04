# Clean up directory after compiling

# Files
del DataSummary.aux
del DataSummary.knit.md
del DataSummary.log
del DataSummary.pdf
del DataSummary.Rmd
del DataSummary.tex
del DataSummary.upa
del DataSummary.upb
del DataSummary.utf8.md
del texput.log

# Folders
rmdir /S /Q knitr-figs-pdf
rmdir /S /Q _book
