# bio.plotting

Oct 17, 2016 - Mike McMahon (mike.mcmahon@dfo-mpo.gc.ca)

**Please refer to [the wiki](https://github.com/Beothuk/bio.plotting/wiki/home) for more extensive documentation.**

This package facilitates the creation of simple basemaps, with the option of adding the contents of a dataframe as point data.  It is intentionally simple, but flexible.

  * No data needs to exist on your computer to generate a map.
  * The map can  focused at any extent in Canada, and includes the US, Greenland and France (for Saint Pierre and Miquelon).
  * The map can be produced in almost any coordinate reference system (CRS) (many of which can be found [here](http://www.epsg-registry.org/)).
  * Any dataframe with coordinates can be added to a map as points.  Point data is always assumed to be in WGS84.
  * Points can be styled to reflect the values they represent via `use.buckets = TRUE` (i.e. bigger, darker symbols reflect larger values)

##Installing this package
In R, you first need to install the devtools package, then you can install this package directly from github.  In practice, this looks like:
```R
> require(devtools)
> install_github('Beothuk/bio.plotting')
Downloading GitHub repo Beothuk/bio.plotting@master
from URL https://api.github.com/repos/Beothuk/bio.plotting/zipball/master
Installing bio.plotting
....
Reloading installed bio.plotting
> 
```

That's it!
Don't forget to `require(bio.plotting)` to use it. Test it by running `make_basemap()`.


