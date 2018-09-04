rdas/pr-daily.rda: wrangling-puerto-rico.R init.R
	R CMD BATCH --no-save wrangling-puerto-rico.R

rdas/pr-icd.rda: wrangling-puerto-rico.R init.R
	R CMD BATCH --no-save wrangling-puerto-rico.R

rdas/florida-daily.rda: wrangling-florida.R init.R
	R CMD BATCH --no-save wrangling-florida.R

rdas/louisiana-monthly.rda: wrangling-louisiana.R init.R
	R CMD BATCH --no-save wrangling-louisiana.R

rdas/nj-monthly.rda: wrangling-new-jersey.R init.R
	R CMD BATCH --no-save wrangling-new-jersey.R 

rdas/fit-models.rda: rdas fit-models.R
	R CMD BATCH --no-save fit-models.R

rdas: rdas/pr-daily.rda rdas/florida-daily.rda rdas/louisiana-monthly.rda rdas/nj-monthly.rda rdas/pr-icd.rda

models: rdas/fit-models.rda

figs/figure-1.pdf: rdas rdas/fit-models.rda figure-1.R
	R CMD BATCH --no-save figure-1.R

figs/figure-2.pdf: rdas rdas/fit-models.rda figure-2.R
	R CMD BATCH --no-save figure-2.R

figs/figure-3.pdf: rdas rdas/fit-models.rda figure-3-table-1.R
	R CMD BATCH --no-save figure-3-table-1.R

figs/figure-fig4.pdf: rdas rdas/fit-models.rda figure-4.R
	R CMD BATCH --no-save figure-4.R

figs/figure-fig5.pdf: rdas rdas/fit-models.rda figure-5.R
	R CMD BATCH --no-save figure-5.R

figs: figs/figure-1.pdf figs/figure-2.pdf figs/figure-3.pdf figs/figure-fig4.pdf figs/figure-fig5.pdf

figs/supp-figure-1.pdf: rdas supp-figure-1.R
	R CMD BATCH --no-save supp-figure-1.R

figs/supp-figure-2.pdf: rdas supp-figure-2.R
	R CMD BATCH --no-save supp-figure-2.R

figs/supp-figure-3.pdf: rdas supp-figure-3.R
	R CMD BATCH --no-save supp-figure-3.R

figs/supp-figure-4.pdf: rdas supp-figure-4.R
	R CMD BATCH --no-save supp-figure-4.R

figs/supp-figure-5.pdf: rdas rdas/fit-models.rda supp-figure-5.R
	R CMD BATCH --no-save supp-figure-5.R

figs/supp-figure-6.pdf: rdas rdas/fit-models.rda supp-figure-6.R
	R CMD BATCH --no-save supp-figure-6.R

figs/supp-figure-7.pdf: rdas rdas/fit-models.rda supp-figure-7.R
	R CMD BATCH --no-save supp-figure-7.R

figs/supp-figure-8.pdf: rdas supp-figure-8.R
	R CMD BATCH --no-save supp-figure-8.R

figs/supp-figure-9.pdf: rdas rdas rdas/fit-models.rda supp-figure-9.R
	R CMD BATCH --no-save supp-figure-9.R

figs/supp-figure-10.pdf: rdas rdas/fit-models.rda figure-5.R
	R CMD BATCH --no-save supp-figure-5.R

figs/supp-figure-11.pdf: rdas rdas/fit-models.rda supp-figure-11.R
	R CMD BATCH --no-save supp-figure-11.R

figs/supp-figure-12.pdf: rdas rdas/pr-daily.rda supp-figure-11.R
	R CMD BATCH --no-save supp-figure-12.R

figs/supp-table-1.html: supp-table-1.R 
	R CMD BATCH --no-save supp-table-1.R

supp-figs: figs/supp-figure-1.pdf figs/supp-figure-2.pdf figs/supp-figure-3.pdf figs/supp-figure-4.pdf figs/supp-figure-5.pdf figs/supp-figure-6.pdf figs/supp-figure-7.pdf figs/supp-figure-8.pdf figs/supp-figure-9.pdf figs/supp-figure-10.pdf figs/supp-figure-11.pdf figs/supp-figure-12.pdf figs/supp-table-1.html


clean:
	rm *.Rout


all: figs supp-figs clean

