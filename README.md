<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/tanaylab/tgconfig.svg?branch=master)](https://travis-ci.org/tanaylab/tgconfig)
[![Codecov test
coverage](https://codecov.io/gh/tanaylab/tgconfig/branch/master/graph/badge.svg)](https://codecov.io/gh/tanaylab/tgconfig?branch=master)
<!-- badges: end -->

tgconfig
========

The goal of tgconfig is to provide infrastructure for managing package
parameters, inspired by [pgkconfig](https://github.com/r-lib/pkgconfig)

Code
----

code can be found at
<a href="https://github.com/tanaylab/tgconfig" class="uri">https://github.com/tanaylab/tgconfig</a>

Installation
------------

``` r
install.packages('tgconfig', repos=c(getOption('repos'), 'https://tanaylab.github.io/repo'))
```

Usage
-----

Parameters are easy to get in relevant functions within a package:

``` r
library(tgconfig)
register_param('param', 'scrdb')
set_param('param', 'value', 'scrdb')
get_param_strict('param', 'scrdb')
#> [1] "value"
```

Error is thrown if a parameter is missing:

``` r
get_param_strict('another', 'scrdb')
#> Error in get_param(param, package = package, fallback = stop(sprintf("there is no parameter \"%s\" in package \"%s\"", : there is no parameter "another" in package "scrdb"
```

Developers are able to register parameters and set their default value
in a config file that is part of the package in YAML format:

    char_param: value
    expr_param: !expr seq(1:5)
    numeric_param: 500
    boolean_param: true

``` r
config_file <- example_config_file()
register_params(config_file, 'scrdb')
get_package_params('scrdb')
#> $param
#> [1] "value"
#> 
#> $char_param
#> [1] "value"
#> 
#> $expr_param
#> [1] 1 2 3 4 5
#> 
#> $numeric_param
#> [1] 500
#> 
#> $boolean_param
#> [1] TRUE
```

Users are able to override parameters using their own YAML:

    char_param: 'user_char'
    expr_param: 'user_exp'
    numeric_param: 700
    boolean_param: false

``` r
override_params(system.file('config/override_example.yaml', package='tgconfig'), package='scrdb')
get_package_params('scrdb')
#> $param
#> [1] "value"
#> 
#> $char_param
#> [1] "user_char"
#> 
#> $expr_param
#> [1] "user_exp"
#> 
#> $numeric_param
#> [1] 700
#> 
#> $boolean_param
#> [1] FALSE
```

Users get an exception when trying to override a parameter that was not
registered:

``` r
set_param('other_param', 'value', 'scrdb')
#> Error in set_param("other_param", "value", "scrdb"): parameter other_param is not registered in package "scrdb"
```

Users can load multiple parameters to the current environment:

``` r
load_params_to_env(c('expr_param', 'boolean_param'), 'scrdb')
expr_param
#> [1] "user_exp"
boolean_param
#> [1] FALSE
```
