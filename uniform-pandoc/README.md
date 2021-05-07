# Package Pandoc in Blog

Pandoc is exetensively used in the conversion of the blog contributions (structured as markdown with metada) to the html and the pdf files in the blog as web presence (including options to print).
The functions collected in this package must not depend on
- the structure of the storage or the presentation of the site, neither before or after the conversion (this is concentrated in, for example, in the modules in SSG).
- specific choices of how the posts are converted.

Therefore:
- the input for the function is either text, json, or absulte path; the results are some text format (text, html).
- details of the conversions are explicitely stated and not controlled values in modules in this package. 

The main functions are:
- md -> docrep: 
    readMarkdown2docrep

    -- issue: docrep structure 
    
    - convTex2pdf, calling 
            - write2pdf (runs lualatex)
    - convPanrep2html, calling 
            - bakeOneFile2panrep 
            - docRep2panrep

Common Structures imported in SSG are: 
import Uniform.PandocImports ( panrepFileType, texSnipFileType )
import Uniform.PandocImports ( panrepFileType, texSnipFileType )
