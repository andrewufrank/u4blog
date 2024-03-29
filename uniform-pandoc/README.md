# Package Pandoc for use in a Static Web Generator (e.g. `daino`)

Pandoc is convenient to used for the conversion of the source of a web page  (structured as markdown with metadata) to the `html` and the `pdf` files in a  web site (including options to print).
The functions collected in this package must not depend on
- the structure of the storage or the presentation of the site, neither before or after the conversion (this is concentrated in, for example, in the modules in `daino`).
- specific choices of how the posts are converted.

Therefore:
- the input for the function is either text, json, or absolute path; the results are some text format (text, html).
- details of the conversions are explicitly stated and not controlled values in modules in this package. 

The main functions are:
- md -> docrep: bakeOneMD2docrep
    readMarkdown2docrep (in Markdown.hs)
    checkDocrep (completes the yaml meta data, 
        the yaml meta data override what is in pandoc)
            -- move from ssg
    addRefs (in Docrep.hs)

    - issue: docrep structure potentially conflict between
        the yaml collected from `md` and values in pandoc meta

- docrep -> panrep: bakeOneDocrep2panrep
    docrep2panrep  
    addindex2yam


- panrep ->     
    - convTex2pdf, calling 
            - write2pdf (runs lualatex)
    - convPanrep2html, calling 
            - bakeOneFile2panrep 
            - docrep2panrep

Common Structures imported in `daino` are: 
import Uniform.PandocImports ( panrepFileType, texSnipFileType )
