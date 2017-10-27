tests:
	raco test test

watch/tests:
	@make tests
	@watchman-make -p 'src/**/*.rkt' 'test/**/*.rkt' -t 'tests'
