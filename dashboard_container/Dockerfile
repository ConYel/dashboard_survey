FROM docker.io/rhub/r-minimal:4.3.1-patched

RUN apk update
RUN apk add --no-cache --update-cache \
    --repository http://nl.alpinelinux.org/alpine/v3.16/main \
    autoconf=2.71-r0 \
    automake=1.16.5-r0 \
    bash tzdata
RUN echo "America/Edmonton" > /etc/timezone

RUN installr -d \
    -t "R-dev file linux-headers libxml2-dev gnutls-dev openssl-dev libx11-dev cairo-dev libxt-dev" \
    -a "icu-libs icu libxml2 cairo libx11 font-xfree86-type1" \
    dplyr@1.1.2 shiny@1.7.5 ggplot2@3.4.3 plotly@4.10.2 stringr@1.5.0 \
    purrr@1.0.1 DT@0.28 shinydashboard@0.7.2  vroom@1.6.3 viridis@0.6.4 Cairo

RUN rm -rf /var/cache/apk/*


RUN addgroup --system app && adduser --system --ingroup app app
COPY Rprofile.site /usr/lib/R/etc/
RUN mkdir /home/dashboard
WORKDIR /home/dashboard
COPY dashboard_survey.R .
RUN chown app:app -R /home/dashboard
USER app

EXPOSE 3838

ENTRYPOINT [ "Rscript", "dashboard_survey.R"]
