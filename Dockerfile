FROM rocker/shiny-verse:latest

LABEL org.opencontainers.image.source https://github.com/Uni-of-Exeter/Planting-Tools

ARG CONDA_PATH=/shared/miniconda

# Set dgpsi path version, BUILD ARG
# curl -sSL https://raw.githubusercontent.com/mingdeyu/dgpsi-R/refs/heads/master/R/initi_py.R | grep "env_name *<-" | grep --invert-match "^\s*#" | grep --only-matching --perl-regexp 'dgp.*\d'
ARG DGPSI_FOLDER_NAME

ARG CONDA_ENV_PATH=${CONDA_PATH}/envs/${DGPSI_FOLDER_NAME}
ARG DEBIAN_FRONTEND=noninteractive

RUN apt update && \
    apt -y --no-install-recommends upgrade && \
    apt -y clean && \
    apt -y autoremove --purge && \
    apt -y autoclean

RUN apt -y --no-install-recommends install git
# For packages
RUN apt -y --no-install-recommends install libcurl4-openssl-dev
RUN apt -y --no-install-recommends install libfontconfig1-dev
RUN apt -y --no-install-recommends install libxml2-dev
RUN apt -y --no-install-recommends install libudunits2-dev
RUN apt -y --no-install-recommends install libssl-dev
RUN apt -y --no-install-recommends install libproj-dev
RUN apt -y --no-install-recommends install cmake
RUN apt -y --no-install-recommends install libgdal-dev
RUN apt -y --no-install-recommends install libharfbuzz-dev
RUN apt -y --no-install-recommends install libfribidi-dev
# For RRembo, it depends on eaf
RUN apt -y --no-install-recommends install libgsl-dev
RUN apt -y --no-install-recommends install libglu1-mesa
# For dgpsi
RUN apt -y --no-install-recommends install libtiff-dev
RUN apt -y --no-install-recommends install libjpeg-dev
# Miniconda only supports s390x and x86_64 (amd64)
# But rocker/shiny-verse only supports amd64
RUN arch=$(uname -p) && \
    if [ "$arch" != "x86_64" ]; then \
        echo "Unsupported architecture: $arch"; \
        exit 1; \
    fi

# Miniconda https://docs.anaconda.com/miniconda/
RUN mkdir -p "${CONDA_PATH}"
RUN arch=$(uname -p) && wget "https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-${arch}.sh" -O "${CONDA_PATH}/miniconda.sh"
RUN bash "${CONDA_PATH}/miniconda.sh" -b -u -p "${CONDA_PATH}"
RUN rm -f "${CONDA_PATH}/miniconda.sh"

RUN apt update && \
    apt -y --no-install-recommends upgrade && \
    apt -y clean && \
    apt -y autoremove --purge && \
    apt -y autoclean

# Install packages while making the image small, and do not reinstall them if they are already there and updated
# RUN Rscript -e "install.packages('remotes', lib = normalizePath(Sys.getenv('R_LIBS_USER')), repos = 'https://cran.rstudio.com/')"
COPY DESCRIPTION .
# Packages update once in a while. We (arbitrarily) update them by invalidating the cache monthly
RUN date +%Y-%m && \
    Rscript -e "install.packages('remotes', repos = 'https://cran.rstudio.com')" && \
    Rscript -e "remotes::install_deps(repos = 'https://cran.rstudio.com')"
RUN rm -f DESCRIPTION

# Make conda command available to all
ARG PATH_DOLLAR='$PATH' # do not interpolate $PATH, this is meant to update path in .bashrc
ARG COMMAND_EXPORT_PATH_BASHRC="export PATH=\"${CONDA_PATH}/bin:${PATH_DOLLAR}\""
# $COMMAND_EXPORT_PATH_BASHRC contains: export PATH="<conda_path>/bin:$PATH"
RUN for userpath in /home/*/ /root/; do \
        echo "${COMMAND_EXPORT_PATH_BASHRC}" | tee -a "${userpath}/.bashrc"; \
    done
# Tell all R sessions about it
RUN echo "options(reticulate.conda_binary = '${CONDA_PATH}/bin/conda')" | tee -a "$R_HOME/etc/Rprofile.site"

# Initialize dgpsi, and say yes to all prompts
RUN Rscript -e "readline<-function(prompt) {return('Y')};dgpsi::init_py()"

# Let users install packages, update package list, search
RUN mkdir -p /etc/sudoers.d
RUN echo "User_Alias MYUSERS = 999 > /etc/sudoers.d/group-rstudio-users"
RUN echo "Cmnd_Alias INSTALL = /usr/bin/apt-get install *, /usr/bin/apt install *" >> /etc/sudoers.d/group-rstudio-users
RUN echo "Cmnd_Alias UPDATE = /usr/bin/apt-get update, /usr/bin/apt update" >> /etc/sudoers.d/group-rstudio-users
RUN echo "Cmnd_Alias UPGRADE = /usr/bin/apt-get upgrade, /usr/bin/apt upgrade" >> /etc/sudoers.d/group-rstudio-users
RUN echo "Cmnd_Alias SEARCH = /usr/bin/apt-get search, /usr/bin/apt search" >> /etc/sudoers.d/group-rstudio-users
RUN echo "Cmnd_Alias REMOVE = /usr/bin/apt-get remove, /usr/bin/apt remove" >> /etc/sudoers.d/group-rstudio-users
RUN echo "Cmnd_Alias AUTOREMOVE = /usr/bin/apt-get autoremove, /usr/bin/apt autoremove" >> /etc/sudoers.d/group-rstudio-users
RUN echo "MYUSERS ALL = INSTALL, UPDATE, UPGRADE, SEARCH, REMOVE, AUTOREMOVE" >> /etc/sudoers.d/group-rstudio-users
