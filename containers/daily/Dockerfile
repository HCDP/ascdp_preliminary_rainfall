# Start with the Rainfall Preliminary Aggregation Base.
FROM ghcr.io/hcdp/task-ascdp-preliminary-rainfall-base:latest
LABEL org.opencontainers.image.source="https://github.com/hcdp/ascdp_preliminary_rainfall"
LABEL org.opencontainers.image.description="American Samoa Daily Rainfall Aggregation and Mapping."

ADD /envs/daily/prod.env /workspace/envs

# Pull the daily dependencies and extract them.
RUN mkdir -p /workspace/dependencies
WORKDIR /workspace/dependencies
RUN wget -nv https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/ASCDP/workflow_data/preliminary/rainfall/dependencies/daily_dependencies.tar.gz
RUN tar -xf daily_dependencies.tar.gz
RUN rm daily_dependencies.tar.gz


RUN mkdir -p workspace/code
ADD /code/daily /workspace/code

# Add upload config
ADD /containers/daily/configs/upload.json /sync/config.json

# Add the task wrapper script.
ADD /containers/daily/task.sh /workspace/task.sh


WORKDIR /workspace
# This container is intended to run the task wrapper at launch, unless otherwise specified at run-time.
CMD [ "/bin/bash", "task.sh" ]