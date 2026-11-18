.PHONY: deps docs

R = Rscript

deps:
	$(R) -e 'pak::local_install_deps(dependencies = c("hard", "Config/Needs/website"))'

docs: deps
	$(R) -e 'pkgdown::build_site()'
