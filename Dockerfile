FROM haskell:9.6.6-slim AS build-deps

WORKDIR /build

COPY overseerr-auto-requester.cabal ./
COPY LICENSE ./

RUN cabal update
RUN cabal build --only-dependencies

FROM build-deps AS build-app

COPY src ./src
RUN cabal build overseerr-auto-requester
RUN find dist-newstyle/build -type f -name overseerr-auto-requester -exec cp {} /build/overseerr-auto-requester \;

FROM debian:bullseye-slim

COPY --from=build /build/overseerr-auto-requester /opt/overseerr-auto-requester
RUN chmod +x /opt/overseerr-auto-requester
RUN apt-get update && apt-get install -y ca-certificates && update-ca-certificates

ENTRYPOINT [ "/opt/overseerr-auto-requester" ]
