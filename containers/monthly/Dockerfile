# Start with the Rainfall Preliminary Aggregation Base.
FROM ghcr.io/hcdp/task-ascdp-preliminary-rainfall-base:latest
LABEL org.opencontainers.image.source="https://github.com/hcdp/ascdp_preliminary_rainfall"
LABEL org.opencontainers.image.description="American Samoa Monthly Rainfall Aggregation and Mapping."

ADD /envs/monthly/prod.env /workspace/envs

# Pull the monthly dependencies and extract them.
RUN mkdir -p /workspace/dependencies
WORKDIR /workspace/dependencies
RUN wget -nv https://api.hcdp.ikewai.org/files/download/ASCDP/workflow_data/preliminary/rainfall/dependencies/monthly_dependencies.tar.gz
RUN tar -xf monthly_dependencies.tar.gz
RUN rm monthly_dependencies.tar.gz


RUN mkdir -p workspace/code
ADD /code/monthly /workspace/code

# Add upload config
ADD /containers/monthly/configs/upload.json /sync/config.json

# Add the task wrapper script.
ADD /containers/monthly/task.sh /workspace/task.sh


WORKDIR /workspace
# This container is intended to run the task wrapper at launch, unless otherwise specified at run-time.
CMD [ "/bin/bash", "task.sh" ]