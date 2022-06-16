
doc: 
	R -s -e "pkgload::load_all('pkg');roxygen2::roxygenize('pkg')"

pkg: doc
	rm -f *.tar.gz
	R CMD build pkg

check: doc
	rm -f *.tar.gz
	R CMD build pkg
	R CMD check *.tar.gz

cran: doc
	rm -f *.tar.gz
	R CMD build pkg
	R CMD check --as-cran *.tar.gz

install: doc
	rm -f *.tar.gz
	R CMD build pkg
	R CMD INSTALL *.tar.gz

test: doc
	R -s -e "tinytest::build_install_test('pkg')"

manual: doc
	R CMD Rd2pdf --force -o manual.pdf ./pkg

revdep: doc
	rm -rf *.tar.gz
	R CMD build pkg
	rm -rf revcheck
	mkdir revcheck
	mv *.tar.gz revcheck
	R -s -e "out <- tools::check_packages_in_dir('revcheck',reverse=list(which='most'),Ncpus=3); print(summary(out)); saveRDS(out, file='revcheck/output.RDS')"

introduction:
	R -s -e "rmarkdown::render('pkg/vignettes/intro.Rmd')"
	xdg-open pkg/vignettes/intro.html

clean:
	rm -rf revdep
	rm -f *.tar.gz
	rm -f pkg/vignettes/*.html
	rm -rf *.Rcheck
	rm -rf manual.pdf
