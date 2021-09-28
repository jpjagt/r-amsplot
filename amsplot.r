"
amsplot.r - plot amsterdam neighborhood data geographically.

date: 2021-09-28
author: jpjagt / july
usage: see corresponding amsplot.Rmd file for examples.

sources:
- https://stackoverflow.com/questions/36678676/how-to-draw-a-heat-map-of-new-york-city-community-based-on-certain-data-using-r
- https://randomjohn.github.io/r-geojson-srt/
- geojson data from https://maps.amsterdam.nl/open_geodata/

on a personal note:

in programming and development, an INCREDIBLE amount of work is done by
open-source developers -- often in their free time, often completely for
free. without their efforts, the process of programming would be WAY more
labourious, and technology would not be as far as it is today; probably, lots
of the software we take for granted would not exist today. the programming
community is heavily based on the open sharing of knowledge and code, and
people helping other people out: you yourself have probably gotten a lot of
help from stackoverflow by now. i, in my five+ years of coding experience, have
used it daily, and continue to do so, and i think that's the case for most
programmers.  i've learnt so much from the community in the last five years,
and have depended so much on the efforts by open-source programmers (by
literally /millions/ of people), that i want to take any chance i have to give
back to the community. (and in this case, that's you, my co-MADE-students!)
that's why i provide this script for you, and why i took extra efforts in
commenting it.  to get the most out of this script, read the code line-by-line,
and try to figure out what it does and why it needs to be there. hopefully, the
comments help out with that.

i encourage you, if you continue in coding endeavours, to give back to the
community! try to answer a stackoverflow question, or make a contribution to an
open-source project. you'll learn from it, and we can continue this magical
cycle of the passing on of knowledge.

also, i'm always happy to help with any questions you have (as long as i get to
finish my code too ;)

love! j
"

# install and load packages

if (!require(geojsonio)) {
    install.packages("geojsonio")
    library(geojsonio)
}
if (!require(rgeos)) {
    install.packages("rgeos")
    library(rgeos)
}
if (!require(maptools)) {
    install.packages("maptools")
    library(maptools)
}
if (!require(ggplot2)) {
    install.packages("ggplot2")
    library(ggplot2)
}

# better color palette
library(viridis)

# we need information which describes the area (as a polygon) of each neighborhood.
# i got this data from the amsterdam municipality data department.
# source: https://maps.amsterdam.nl/open_geodata/
# at Algemeen -> Buurten (excl. water) -> GeoJSON LngLat
# that downloads the json file which is opened below.
# this json file describes the neighborhood areas in a format called "geojson".
# in R, there is (somebody made!) a package which makes it easy to work with
# geojson, called geojsonio.
# geojsonio does a lot of stuff behind the scenes which we then don't need to worry about! yay

# the name of the file.
ams_neighborhoods_file <- "./amsterdam_neighborhoods.json"
# read the file using the geojsonio package.
ams_neighborhoods <- geojson_read(ams_neighborhoods_file, what = "sp")

# fortify is a function from geojsonio which constructs a dataframe
# from the geojson data. not sure what it does exactly, but this is done
# in the stackoverflow question, so i do it too.
ams_neighborhoods_map <- fortify(ams_neighborhoods, region="Buurt")
# if we don't lowercase the neighborhood names, weird stuff happens later on.
ams_neighborhoods_map$id <- tolower(ams_neighborhoods_map$id)

# some columns are categorical rankings (i.e. containing values like
# "1. low" and "2. high".
# our plot needs continuous values (decimal numbers) to work properly,
# so this is a convenience function which converts such a categorical ranking
# to numerical values ("1. low" would become 1, etc).
categorical_ranking2number <- function(categorical_ranking) {
    # just take the first character, and convert it to integer.
    strtoi(substr(categorical_ranking, 0, 1))
}


amsplot_col <- function(df, col_name, normalize_by=NULL, outline_color="#2b2b2b") {
    "
    plot one column from any dataframe containing at least:
    - a neighborhood column
    - that column (which should be numerical)

    parameters:
    - df: the input df.
    - col_name: the name of the column in the df which you want to plot.
    - normalize_by: an optional parameter, indicating a column by which
      the data should be normalized (divided by that column).
      useful to show the fraction between two columns (col_name / normalize_by).
    - outline_color: the hex code of the outline color (default is dark gray).

    example usage (plotting the hou_value column for 2019):
    df_housing_2019 <- df_housing[df_housing$year == 2019,]
    print(amsplot_col(df=df_housing_2019, col_name='hou_value'))
    "

    # we can obviously just show a single value per area.
    # therefore, there can't be multiple values for one neighborhood.
    # if this is the case (if there are multiple entries anywhere in the
    # neighborhood column), we raise an error telling the user.
    if (any(duplicated(df$neighborhood))) {
        stop("there can be no duplicate values in the neighborhood column. maybe you forgot to select a single year first?")
    }

    # we want to provide some label for the colorbar legend.
    # it can just be the column name.
    label <- col_name

    # copy the dataframe, because otherwise we would overwrite columns.
    df <- data.frame(df)

    # the neighborhood column needs to be all lowercase,
    # so let's do that conversion, just in case.
    df$neighborhood <- tolower(df$neighborhood)

    # check if the normalize_by has been passed.
    # its default value is NULL, so if it's not NULL, then
    # we know that we should normalize.
    if (!is.null(normalize_by)) {
        # normalize by dividing the target column by the normalizing column.
        # for now, we save it in the dataframe, but that's okay:
        # we copied it in this function and so, won't overwrite any of the
        # original data.
        df[[col_name]] <- df[[col_name]] / df[[normalize_by]]

        # update the label to also denote the normalizing column.
        # paste joins strings together so the label becomes:
        # col_name/normalize_by.
        label <- paste(label, "/", normalize_by, sep="")
    }

    # the code below is mainly adapted from that stackoverflow question
    # listed at the top. i mostly had to rename some things around to make it work.

    # create a new ggplot instance.
    gg <- ggplot()
    # plot all the neighborhoods in the background (so we see their outline,
    # even if the neighborhoods are not present in the data).
    # we just want their outline, and so,
    # we don't fill the shapes with a colour (with fill=NA).
    gg <- gg + geom_map(data=ams_neighborhoods_map, map=ams_neighborhoods_map,
                        aes(x=long, y=lat, map_id=id),
                        color=outline_color, size=0.15, fill=NA)
    # plot the actual data.
    # for each neighborhood, we want to fill the corresponding area with a colour
    # which is based on the value in col_name (for that neighborhood).
    # map_id="neighborhood" means that it should use the "neighborhood" column
    # to match each row in df with an area.
    # indicating fill=col_name indicates to ggplot something like
    # "the fill colour of the area should be determined by the value of col_name".
    gg <- gg + geom_map(data=df, map=ams_neighborhoods_map,
                        aes_string(fill=col_name, map_id="neighborhood"),
                        color=outline_color, size=0.15)

    # finally, some aesthetic stuff. try commenting each of the lines
    # and see what happens to the plot.

    # use the viridis colour palette / gradient.
    gg <- gg + scale_fill_viridis(name=label)
    # add coordinates / grid.
    gg <- gg + coord_map()
    # position the legend at (x, y). try changing these numbers.
    gg <- gg + theme(legend.position=c(0.1,0.2))

    # here, i could have chosen to display the plot with print(gg)
    # or to save it to a file with ggsave("filename.png").
    # however, it is really the responsibility of the user of this
    # function to decide what to do with the plot.
    # so, i just return the plot instance to the user.
    # in general, it is very useful to create functions that do not
    # do too much, because it makes them more reusable and easier
    # to understand. always ask yourself: is this /really/ a core
    # part of what the function should do?
    gg
}
