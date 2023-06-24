# Changelog uniform-pandoc
  0.0.2.1 for cabal may 2021 8.10.4  
  0.0.2.2 systematic bakeOneX2y function names  
  0.0.2.3 biblio-refs fixing, not yet change to citeproc-hs  
            currently fixed to old version of pandoc-types   
            processPDF.hs replaced with uniform-latex2pdf  
  0.0.2.4 for ghc 9.2.1 (first for 8.10.7 limits on rpi64)  
            uses pandoc (and the Pandoc.Citeproc code  
            not pandoc-citeproc.hs)  
  0.0.2.5 adaption to use of ExceptionT  
  
0.1.5 branch for ghc 9.2.5  
0.1.5.1  
0.1.5.2 removed pandoc2pdf which was used for an experiment to directly convert to pdf using the pandoc internal process. 
        now use uniform-latex2pdf (which uses lualatex separately)
0.1.5.3 include Ext_latex_macros to to parse latex math

