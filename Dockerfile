FROM gerbil/gerbilxx:master AS builder

RUN apk add --no-cache qt6-qtbase-dev

ENV GERBIL_LOADPATH=/root/.gerbil/lib
ENV USER=root

WORKDIR /src
COPY . .
RUN make build && make install

# ---------- test target ----------
FROM builder AS test
RUN QT_QPA_PLATFORM=offscreen make test
