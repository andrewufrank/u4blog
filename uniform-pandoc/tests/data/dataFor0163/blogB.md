---
title: "B title missing"
abstract: |
     This blog uses a reference given locally. 
     second line 
 
# author: AOS
bibliography: resources/BibTexLatex.bib
date: 2010-07-29
keywords: referenceTest

# references:
# - author:
#   - family: Fenner
#     given: Martin Beat
#   container-title: Nature Materials
#   id: fenner2012a
#   issued:
#     month: 3
#     year: 2212
#   page: 261-263
#   publisher: Nature Publishing Group
#   title: One-click science marketing
#   type: article-journal
#   volume: 11
style: resources/chicago-fullnote-bibliography-bb.csl
# styleBiber: athorYear
reference-section-title: References

version: private # publish
# issue with lualatex: Not writing to .bib (openout_any = p).

# # visibility: public
# headerShift: zero
---

# An example with local references details

The [@frank-machbarkeit] and the next in a foonote^[with a text[@frank09geo] before and after] are in the biblio file, but the reference [@fenner2012a] is given in the file locally (note the format and keywords!).