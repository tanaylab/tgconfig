config <- new.env()

# config internal functions
set_config <- function(param, value, package){
	if (is.null(config[[package]])){
		config[[package]] <- list()
	}
	config[[package]][[param]] <- value
}

get_config <- function(param, package){
	config[[package]][[param]]
}

#' Guess package the code is invoked from
#'
#' @param env environment
#'
#' @return name of the package the code is invoked from, and NULL if it is not run from a package
#'
#' @examples
#' guess_package(environment())
guess_package <- function(env){
	package <- utils::packageName(env=parent.frame(n=2))
	if (is.null(package)){
		stop('Please provide package name')
	}
	return(package)
}

# parameters set and get

#' Get package parameter
#' @param param parameter to get
#'
#' @param package package of the parameter (NULL if running from a package)
#' @param fallback what to do if parameter not found
#'
#' @return value of \code{param} in package \code{package} and \code{fallback} if parameter not found
#'
#' @examples
#'
#' register_param('param1', 'tgconfig')
#' set_param('param1', 'value1', 'tgconfig')
#' get_param('param1', 'tgconfig')
#'
#' # try to get a parameter that doesn't exist
#' get_param('other_param', 'tgconfig')
#'
#' # sometimes we want to throw and error if the parameter doesn't exist
#' get_param('other_param', 'tgconfig', fallback=stop())
#'
#'
#' @export
get_param <- function(param, package=NULL, fallback=NULL){
	package <- package %||% guess_package(parent.frame(n=2))
	res <- get_config(param, package)
	if (is.null(res)){
		fallback
	} else {
		res
	}
}

#' Set package parameter
#' @param param parameter to set. An error would be thrown if parameter is not registered.
#'
#' @param value value to set the parameter to
#' @param package package of the parameter (NULL if running from a package)
#'
#' @examples
#' register_param('param1', 'tgconfig')
#' set_param('param1', 'value1', 'tgconfig')
#' get_param('param1', 'tgconfig')
#'
#' # try to set a parameter that doesn't exist
#' set_param('other_param', 'tgconfig')
#'
#' @seealso register_param, get_param
#'
#'
#' @export
set_param <- function(param, value, package=NULL){
	package <- package %||% guess_package(parent.frame(n=2))

	params <- list_package_params(package)
	if (param %in% params){
		set_config(param, value, package)
	} else {
		stop(sprintf('paramter %s is not registered in package "%s"', param, package))
	}
}

#' Get all package parameters
#' @param package package
#'
#' @return a list with package parameters and values. NULL if \code{package} has no parameters
#'
#' @examples
#' register_param('param1', 'tgconfig')
#' register_param('param2', 'tgconfig')
#' set_param('param1', 'value1', 'tgconfig')
#' set_param('param2', 'value2', 'tgconfig')
#' get_package_params('tgconfig')
#'
#' @export
get_package_params <- function(package){
	config[[package]]
}

#' List package parameters
#' @param package package
#'
#' @return names of package parameters. NULL if \code{package} has no parameters
#'
#' @examples
#' register_param('param1', 'tgconfig')
#' register_param('param2', 'tgconfig')
#' set_param('param1', 'value1', 'tgconfig')
#' set_param('param2', 'value2', 'tgconfig')
#' list_package_params('tgconfig')
#'
#' @export
list_package_params <- function(package){
	names(config[[package]])
}

#' Check if package has a paramter
#' @param param parameter
#'
#' @param package package
#'
#' @examples
#' register_param('param1', 'tgconfig')
#' has_param('param1', 'tgconfig')
#' has_param('param2', 'tgconfig')
#'
#' @export
has_param <- function(param, package=NULL){
	package <- package %||% guess_package(parent.frame(n=2))
	return(!is.null(get_param(param=param, package=package)))
}

#' Register a paramter to package
#'
#' @param param parameter to register
#'
#' @param package package to register parameter to
#' @param default_value default value of the parameter (default: NA)
#'
#' @examples
#' register_param('param1', 'tgconfig')
#' get_package_parame('tgconfig')
#'
#' @export
register_param <- function(param, package=NULL, default_value=NA){
	package <- package %||% guess_package(parent.frame(n=2))
	set_config(param, default_value, package)
}


# read from config files

#' Override pre-set parameters from config file
#'
#' @param config_file yaml file with paramters and values
#'
#' @param package package
#'
#' @examples
#' config_file <- example_config_file()
#' register_params(config_file, 'tgconfig')
#' get_package_params('tgconfig')
#' override_params(system.file('config/override_example.yaml', package='tgconfig'))
#' get_package_params('tgconfig')
#'
#'
#' @export
override_params <- function(config_file, package=NULL){
	package <- package %||% guess_package(parent.frame(n=2))

	for (conf_file in config_file){
		conf <- yaml::yaml.load_file(config_file)
		params <- names(conf)
		for (i in 1:length(conf)){
			set_param(params[i], conf[[params[i]]], package=package)
		}
	}

}

#' Register parameters from config file
#'
#' @param config_file yaml file with paramters and values
#'
#' @param package package
#'
#' @examples
#' config_file <- example_config_file()
#' register_params(config_file, 'tgconfig')
#' get_package_params('tgconfig')
#'
#' @export
register_params <- function(config_file, package=NULL){
	package <- package %||% guess_package(parent.frame(n=2))

	for (conf_file in config_file){
		conf <- yaml::yaml.load_file(config_file)
		params <- names(conf)
		for (i in 1:length(conf)){
			register_param(params[i], default_value=conf[[params[i]]], package=package)
		}
	}
}

#' Get example config file path
#'
#' @export
example_config_file <- function(){
	system.file('config/example.yaml', package='tgconfig')

}


#' Dump example config file
#'
#' @param path path to dump example config file to
#'
#' @export
dump_example_config <- function(path){
	file.copy(example_config_file(), path)
}


# Utils
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) { lhs } else { rhs }
}
