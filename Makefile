.PHONY: deps docs test

R = Rscript

deps:
	$(R) -e 'pak::local_install_deps(dependencies = c("hard", "Config/Needs/website"))'

docs: deps
	$(R) -e 'pkgdown::build_site()'

test:
	$(R) -e 'testthat::test_local()'
