## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission of a new release.

## The following comments were received upon second CRAN submission

* > Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited. e.g.:
  > ...
  > oldpar <- par(no.readonly = TRUE) # code line i
  > on.exit(par(oldpar)) # code line i + 1
  > ...
  > par(mfrow=c(2,2)) # somewhere after
  > ...
  > e.g.: R/parameter_scan.R
  > If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.

* > Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos. --> inst/doc/growR.R
  > e.g.:
  > olddir <- getwd()
  > setwd(...)
  > ...
  > setwd(olddir)

* > Please proof-read your description text.
  > Currently it reads: "... Run grass growth simulations using a grass growth model based off of ModVege ..."
  > Probably it should be: "... Run grass growth simulations using a grass growth model based on ModVege ..."


## The following comments were received upon first CRAN submission

* > The MIT license file should only contain the two lines:
  > YEAR:
  > COPYRIGHT HOLDER:
  fixed in 3da9768.

* > You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object.
  > Instead of print()/cat() rather use message()/warning() or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console.
  > (except for print, summary, interactive functions)
  Addressed in a178d3f.
  All instances where `print` was still used are now wrapped by `logger()` 
  which now uses `message` instead of `print`. Therefore all output can now 
  easily be suppressed using `set_growR_verbosity()` or by suppressing 
  `messages`.

* > Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies.
  > Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().
  Addressed in 98b7801
  Default paths have been removed in all instances where writing to user's 
  home file space is possible.

* > Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited. e.g.:
  > ...
  > oldpar <- par(no.readonly = TRUE) # code line i
  > on.exit(par(oldpar)) # code line i + 1
  > ...
  > par(mfrow=c(2,2)) # somewhere after
  > ...
  > e.g.: R/modvegesite.R ; R/parameter_scan.R
  > If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.
  Addressed in b77cdcc.
  Thanks for pointing me to on.exit()!

* > Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos. --> inst/doc/growR.R
  > e.g.:
  > olddir <- getwd()
  > setwd(...)
  > ...
  > setwd(olddir)
  Also addressed in b77cdcc, as the vignette code in inst/doc/growR.R made 
  use of ModvegeSite$plot() which changed par().

