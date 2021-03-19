FROM  adoptopenjdk/openjdk11:jdk-11.0.8_10-ubuntu-slim

ARG project_version

WORKDIR /app
ADD target/universal/fpchat-server-$project_version.tgz /app
RUN mv /app/fpchat-server-$project_version /app/fpchat-server

ENTRYPOINT ["/app/fpchat-server/bin/fpchat-server"]