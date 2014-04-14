# Your Static Site in R

## Philosophy

This is a (very basic) [static website](http://en.wikipedia.org/wiki/Static_web_page "Static web page - Wikipedia, the free encyclopedia") generator using R. There are [many](http://staticsitegenerators.net/ "Static Site Generators") such generators, some of which handle R code. This is aimed at making R code a first class component of the site. It uses tools available as R packages.

A static website generator takes a bunch of files, usually written in various templating languages, and generates HTML webpages from it. Those HTML files can then be uploaded to any web server. This website generator supports R-centric templating languages ([brew](http://cran.r-project.org/web/packages/brew/index.html "CRAN - Package brew"), [Rmarkdown](http://www.rstudio.com/ide/docs/authoring/using_markdown "RStudio"), Rhtml). In terms of how it works, in takes (heavy) inspiration from [Middleman](http://middlemanapp.com/).

## Installing

The package is not on CRAN (yet) but you can install it using the [devtools package](http://cran.r-project.org/package=devtools "CRAN - Package devtools")

    library("devtools")
    install_github("yssr", "jiho")

## Basic usage

Load the package and initialise the website

    library("yssr")    
    init("foo")
    
This creates a directory called `foo` and sets your R working directory there. Inside `foo` are:

*   a layout file (`source/layouts/main.brew`) which defines the skeleton of every page on your site.
*   a content file (`source/index.Rmd`) which contains the content of the future `index.html` page

Now turn this into a website using

    render()

This renders "index.Rmd" into HTML code which is then injected in the layout. The result is stored in the newly created `build` directory, next to `source`. To view this in a browser, provided you have python with SimpleHTTPServer, use:

    serve()

## Principle

The render functions does this sequence of things:

1. record all files present in the `source` directory
2. call all R scripts; those scripts may create new content
3. get all template files (`brew`, `Rmd`, `Rhml`), render the content of each one and inject it into the layout. Store the resulting HTML files in the source directory.
4. move everything to the build directory
5. remove template files from `build` because they are not needed on the actual site
6. remove newly created files (HTML but also files created by R scripts) from the `source` directory, to come back to the state before 1.
