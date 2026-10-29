RSCRIPT ?= Rscript
PY ?= python3

.PHONY: restore wos rw rw-fetch targets targets-fetch validation pilot openalex-fetch openalex match analysis sdid figures tables manuscript paper format lint test check ci-docker clean

restore:
	$(RSCRIPT) --vanilla -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org"); renv::load(project = getwd()); renv::restore(prompt = FALSE)'

wos:
	$(PY) -m scripts.wos.01_import

rw:
	$(PY) -m scripts.retraction_watch.01_get --offline
	$(PY) -m scripts.retraction_watch.02_frame
	$(PY) -m scripts.retraction_watch.03_sample

rw-fetch:
	$(PY) -m scripts.retraction_watch.01_get

targets: wos rw
	$(PY) -m scripts.openalex.01_targets --offline

targets-fetch: wos rw
	$(PY) -m scripts.openalex.01_targets

validation: targets
	$(PY) -m scripts.openalex.02_histories --offline --only wos
	$(PY) -m scripts.wos.02_compare
	$(RSCRIPT) scripts/wos/compare.R

validation-fetch: targets
	$(PY) -m scripts.openalex.02_histories --only wos

pilot: targets
	$(PY) -m scripts.openalex.02_histories --pilot
	$(PY) -m scripts.openalex.03_pools --pilot
	$(PY) -m scripts.openalex.04_candidates --pilot

openalex-fetch: targets
	$(PY) -m scripts.openalex.02_histories
	$(PY) -m scripts.openalex.03_pools
	$(PY) -m scripts.openalex.04_candidates
	$(PY) -m scripts.openalex.05_world_counts

openalex: targets
	$(PY) -m scripts.openalex.02_histories --offline
	$(PY) -m scripts.openalex.03_pools --offline
	$(PY) -m scripts.openalex.04_candidates --offline
	$(PY) -m scripts.openalex.05_world_counts --offline

match: openalex
	$(PY) -m scripts.matching.01_match
	$(PY) -m scripts.matching.02_panel

analysis: match
	RSCRIPT='$(RSCRIPT)' $(PY) -m scripts.analysis.01_estimate

sdid: match
	RSCRIPT='$(RSCRIPT)' $(PY) -m scripts.analysis.02_sdid

figures: analysis sdid
	$(RSCRIPT) scripts/figures.R

tables: analysis sdid validation
	$(RSCRIPT) scripts/tables.R

manuscript:
	cd ms && latexmk -xelatex -interaction=nonstopmode -halt-on-error main.tex

paper: figures tables
	$(MAKE) manuscript

format:
	$(RSCRIPT) -e 'for (p in c("R", "scripts", "tests")) styler::style_dir(p)'
	$(PY) -m black scripts tests
	$(PY) -m isort --profile black scripts tests

lint:
	$(PY) -m black --check scripts tests
	$(PY) -m isort --check-only --profile black scripts tests
	$(PY) -m flake8 --max-line-length=100 scripts tests
	$(RSCRIPT) -e 'l <- unlist(lapply(c("R", "scripts", "tests"), lintr::lint_dir), recursive = FALSE); print(l); quit(status = as.integer(length(l) > 0))'

test: wos rw openalex match analysis sdid validation
	$(PY) -m unittest discover -s tests -p 'test_*.py'
	$(RSCRIPT) -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

check: paper lint test

ci-docker:
	docker run --rm -v "$(CURDIR):/project" -w /project \
		-v r_renv_cache:/root/.cache/R/renv \
		-e RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/latest \
		rocker/verse:4.6.0 bash -c "apt-get update && apt-get install -y python3-pip && pip3 install --break-system-packages -r requirements.txt && tlmgr install latexmk fontspec booktabs float caption natbib xurl setspace adjustbox microtype && make restore check"

clean:
	cd ms && latexmk -C main.tex
