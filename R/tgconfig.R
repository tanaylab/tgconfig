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

guess_package <- function(env){	
	package <- utils::packageName(env=parent.frame())
	if (is.null(package)){
		stop('Please provide package name')
	}
	return(package)	
}

# parameters set and get
#' @export
get_param <- function(param, package=NULL, fallback=NULL){
	package <- package %||% guess_package(parent.frame())
	res <- get_config(param, package)
	if (is.null(res)) fallback else res
}

#' @export
set_param <- function(param, value, package=NULL){
	package <- package %||% guess_package(parent.frame())

	params <- list_package_params(package)
	if (param %in% params){
		set_config(param, value, package)
	} else {
		stop(sprintf('paramter %s is not registered in package "%s"', param, package))
	}
}

#' @export
get_package_params <- function(package){	
	config[[package]]
}

#' @export
list_package_params <- function(package){	
	names(config[[package]])
}

#' @export
has_param <- function(param, package=NULL){
	package <- package %||% guess_package(parent.frame())
	return(!is.null(get_param(param=param, package=package)))
}

#' @export
register_param <- function(param, default_value=NA, package=NULL){
	package <- package %||% guess_package(parent.frame())
	set_config(param, default_value, package)
}



# read from config files

#' @export
override_params <- function(config_file, package=NULL){
	package <- package %||% guess_package(parent.frame())

	for (conf_file in config_file){
		conf <- yaml::yaml.load_file(config_file)
		params <- names(conf)
		for (i in 1:length(conf)){		
			set_param(params[i], conf[[params[i]]], package=package)
		}	
	}
	
}

#' @export
register_params <- function(config_file, package=NULL){
	package <- package %||% guess_package(parent.frame())

	for (conf_file in config_file){
		conf <- yaml::yaml.load_file(config_file)
		params <- names(conf)
		for (i in 1:length(conf)){		
			register_param(params[i], conf[[params[i]]], package=package)
		}	
	}
}


# Utils
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) { lhs } else { rhs }
}


