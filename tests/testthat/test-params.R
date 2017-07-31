context("params")

test_that("basic set and get works", {
	register_param('param1', 'tgconfig')
	set_param('param1', 'value1', 'tgconfig')
	expect_equal(get_param('param1', 'tgconfig'), 'value1')  
})

test_that("set and get from package works", {
	register_param('param2')
	expect_equal(get_param('param2'), NA)
	set_param('param2', 'value2')
	expect_equal(get_param('param2'), 'value2')  
})

test_that("set without register fails", {	
	expect_error(set_param('param', 'value'))
	expect_error(set_param('param', 'value'))
})

test_that("get of non existing parameter", {	
	expect_error(get_param_strict('param', 'value'))
	expect_null(get_param('param', 'value'))
})

test_that("register twice does not override", {	
	register_param('param_override', default_value='defval')
	expect_equal(get_param('param_override'), 'defval')
	register_param('param_override', default_value='another')
	expect_equal(get_param('param_override'), 'defval')	
})

test_that("register from file works", {		
	register_params(example_config_file())
	expect_equal(get_param('char_param'), 'value')
	expect_equal(get_param('expr_param'), seq(1:5))
	expect_equal(get_param('numeric_param'), 500)
	expect_equal(get_param('boolean_param'), TRUE)
})

test_that("override from file works", {		
	register_params(example_config_file())
	override_params(system.file('config/override_example.yaml', package='tgconfig'))
	expect_equal(get_param('char_param'), 'user_char')
	expect_equal(get_param('expr_param'), 'user_exp')
	expect_equal(get_param('numeric_param'), 700)
	expect_equal(get_param('boolean_param'), FALSE)
})

test_that("override from file with unknown params fails", {		
	register_params(example_config_file())
	expect_error(override_params(system.file('config/override_fail_example.yaml', package='tgconfig')))	
})

test_that('load_params_to_env loads parameters to env', {
	register_params(example_config_file())
	load_params_to_env(c('expr_param', 'boolean_param', 'numeric_param', 'char_param'))
	expect_equal(char_param, 'user_char')
	expect_equal(expr_param, 'user_exp')
	expect_equal(numeric_param, 700)
	expect_equal(boolean_param, FALSE)
})