# sxcdashboard

Check that you have installed `devtools`:

```r
if (!require("devtools")) install.packages("devtools")
```

Then clone this repo, open the project in RStudio, and run:

```r
# just to be sure that everything is up to date
devtools::update_packages()

# install dependencies
devtools::install()

# either load the package in development mode
devtools::load_all()
run_app()

# or load the package in production mode
library(sxcdashboard)
run_app()
```
