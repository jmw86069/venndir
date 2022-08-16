
# Notes installing Github dev version of sf

Edzer Pebesma graciously updated sf to correct `st_bbox()`

## Install dev sf from Github

### Github instructions for Mac OS

* See: https://github.com/r-spatial/sf
* It requires Mac homebrew

```
brew update
brew install gdal
```

* Note GDAL has fairly large dependencies, including libx11, Qt5, python newt.
* It took about ~5 minutes to install dependencies.
* install `sf` from Github:

```
remotes::install_github("r-spatial/sf", configure.args = "--with-proj-lib=/usr/local/lib/")
```

* The step above failed in the active R session, I started a new
R session in a new shell, in case it was required for software libraries
to be recognized.
