## R CMD check results

0 errors | 0 warnings | 0-2 notes

* ```
  checking CRAN incoming feasibility ...Warning: unable to access index for repository https://bioconductor.org/packages/3.16/bioc/src/contrib:
    cannot open URL 'https://bioconductor.org/packages/3.16/bioc/src/contrib/PACKAGES'
  Warning: unable to access index for repository https://bioconductor.org/packages/3.16/data/annotation/src/contrib:
    cannot open URL 'https://bioconductor.org/packages/3.16/data/annotation/src/contrib/PACKAGES'
  Warning: unable to access index for repository https://bioconductor.org/packages/3.16/data/experiment/src/contrib:
    cannot open URL 'https://bioconductor.org/packages/3.16/data/experiment/src/contrib/PACKAGES'
  Warning in url(sprintf("%s/src/contrib/PACKAGES.gz", u), "rb") :
    URL 'https://mghp.osn.xsede.org/bir190004-bucket01/archive.bioconductor.org/packages/3.16/bioc/src/contrib/PACKAGES.gz': Timeout of 60 seconds was reached
  NB: need Internet access to use CRAN incoming checks
   NOTE
  Maintainer: ‘Kevin Kramer <kevin.pasqual.kramer@protonmail.ch>’
   
   Possibly misspelled words in DESCRIPTION:
     Baumont (13:48)
     Carrère (13:32)
     Jouven (13:17)
     ModVege (11:47, 13:8, 17:73)
  ```

  bioconductor seems to be unreachable from my location. However, growR does 
  not depend or (to my knowledge) interact with bioconductor at all, so this 
  should not be cause for an issue.
  The spelling of these words (names) is fine.
   
* ```
  Found the following (possibly) invalid URLs:
     URL: https://rmets.onlinelibrary.wiley.com/doi/abs/10.1002/joc.2419
       From: man/willmott.Rd
       Status: 403
       Message: Forbidden
  ```

  It seems that Wiley has recently started to use more extensive CAPTCHA and 
  similar human-verification checks. This seems to sometimes (and only 
  sometimes) prevent R CMD CHECK from accessing these links.

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

