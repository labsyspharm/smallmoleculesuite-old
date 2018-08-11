FROM ucbd2k/smallmoleculesuite:base

COPY shiny-server.conf /etc/shiny-server/
COPY extra/bookmark-state.R /
COPY extra/bookmark-state-local.R /
COPY extra/.awspass.* /

RUN git clone https://github.com/rstudio/shiny.git
RUN cd shiny && git checkout 0bb53e8ca5b8e769b11a0ec37a5b5c66eceb9a54 && mv /*.R R/ && cd .. 
RUN R CMD build shiny && R CMD INSTALL shiny_1.1.0.9000.tar.gz

RUN git clone https://github.com/sorgerlab/smallmoleculesuite.git
RUN cd smallmoleculesuite && git checkout shinyapps.io && cd ..
RUN mv /.awspass smallmoleculesuite/SimilaritySelectR/

RUN rm -Rf /srv/shiny-server/*
RUN mv smallmoleculesuite/hms_small_mol /srv/shiny-server
RUN mv smallmoleculesuite/SelectivitySelectR /srv/shiny-server
RUN mv smallmoleculesuite/SimilaritySelectR /srv/shiny-server
RUN mv smallmoleculesuite/LibraryR /srv/shiny-server
RUN rm -Rf smallmoleculesuite/*