FROM haskell:9.6.6-slim AS build-deps

WORKDIR /build

COPY overseerr-auto-request.cabal ./
COPY LICENSE ./

RUN cabal update
RUN cabal build --only-dependencies

FROM build-deps AS build-app

COPY src ./src
RUN cabal build overseerr-auto-request
RUN find dist-newstyle/build -type f -name overseerr-auto-request -exec cp {} /build/overseerr-auto-request \;

FROM debian:bullseye-slim

COPY --from=build-app /build/overseerr-auto-request /opt/overseerr-auto-request
RUN chmod +x /opt/overseerr-auto-request
RUN apt-get update && apt-get install -y ca-certificates && update-ca-certificates

ENTRYPOINT [ "/opt/overseerr-auto-request" ]
