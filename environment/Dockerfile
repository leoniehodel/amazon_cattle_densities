# hash:sha256:16ccfa59ea0f61cf7d63f7a261ada452d69f1537260d358782f2e9e86348a38a
FROM registry.codeocean.com/codeocean/r-studio:2022.07.0-548-r4.2.1-ubuntu18.04

RUN Rscript --no-save --no-restore -e '\
  remotes::install_version("broom", "1.0.7", repos="https://cloud.r-project.org"); \
  remotes::install_version("texreg", "1.39.4", repos="https://cloud.r-project.org"); \
  remotes::install_version("fixest", "0.12.1", repos="https://cloud.r-project.org"); \
  remotes::install_version("modelsummary", "2.4.0", repos="https://cloud.r-project.org")'

