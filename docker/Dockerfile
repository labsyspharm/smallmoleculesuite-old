FROM ucbd2k/smallmoleculesuite:base

COPY shiny-server.conf /etc/shiny-server/

RUN git clone https://github.com/sorgerlab/smallmoleculesuite.git
RUN cd smallmoleculesuite && git checkout shinyapps.io && cd ..

RUN rm -Rf /srv/shiny-server/*
RUN mv smallmoleculesuite/hms_small_mol /srv/shiny-server
RUN mv smallmoleculesuite/SelectivitySelectR /srv/shiny-server
RUN mv smallmoleculesuite/SimilaritySelectR /srv/shiny-server
RUN mv smallmoleculesuite/LibraryR /srv/shiny-server
