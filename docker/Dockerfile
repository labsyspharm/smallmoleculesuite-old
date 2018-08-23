FROM ucbd2k/smallmoleculesuite:shinyapps.io_base

COPY shiny-server.conf /etc/shiny-server/
COPY .aws* /

RUN git clone https://github.com/sorgerlab/smallmoleculesuite.git && \
    cd smallmoleculesuite && git checkout shinyapps.io && cd ..  && \
    cp /.aws* smallmoleculesuite/SimilaritySelectR/ && \
    cp /.aws* smallmoleculesuite/SelectivitySelectR/ && \
    mv /.aws* smallmoleculesuite/LibraryR/ && \
    rm -Rf /srv/shiny-server/* && \
    mv smallmoleculesuite/hms_small_mol /srv/shiny-server && \
    mv smallmoleculesuite/SelectivitySelectR /srv/shiny-server && \
    mv smallmoleculesuite/SimilaritySelectR /srv/shiny-server && \
    mv smallmoleculesuite/LibraryR /srv/shiny-server && \
    rm -Rf smallmoleculesuite