FROM docker.io/amd64/eclipse-temurin

ENV SHINY_HASH f1fe52ed886325de8fd7b683eb2fd678073f023c4c22a40ee6a9e47e760505ba

RUN mkdir -p /opt/shinyproxy/
RUN wget  https://shinyproxy.io/downloads/shinyproxy-3.0.2.jar -O /opt/shinyproxy/shinyproxy.jar
RUN echo "${SHINY_HASH} /opt/shinyproxy/shinyproxy.jar" | sha256sum -c
#COPY application.yml /opt/shinyproxy/application.yml

WORKDIR /opt/shinyproxy/
CMD ["java", "-jar", "/opt/shinyproxy/shinyproxy.jar"]
