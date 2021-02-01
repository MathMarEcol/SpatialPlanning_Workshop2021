all: clean data test pdf site

clean:
	@rm -rf _book
	@rm -rf _bookdown_files
	@rm -f data.zip
	@rm -rf data

clean_temp:
	@rm -f prioritizr-workshop-manual.Rmd
	@rm -f prioritizr-workshop-manual-teaching.Rmd

data:
	mkdir -p data
	Rscript -e "source('data.R')"
	zip -r data.zip data
	rm -rf data

site: clean_temp
	Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"

pdf: clean_temp
	Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book')"
	rm -f prioritizr-workshop-manual.log

purl:
	Rscript -e "knitr::purl('prioritizr-workshop-manual.Rmd')"

check:
	R -e "source('verify-solutions.R')"
	rm -f Rplots.pdf

.PHONY: data clean check website site data
