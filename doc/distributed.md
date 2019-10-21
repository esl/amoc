## Set-up distributed environment with docker

To build a Docker image with Amoc, run the following command from the root of
the repository:
```
docker build -f Dockerfile -t amoc_image:tag .
```
It is important to start building at project root
(it is indicated with dot `.` at the end of command).
It will set build context at the project root.
Dockerfile commands expects a context to be set like that:
 - it copies the **current** source code into the container to compile it.
 - It looks for files in `docker/` relative path.

When the image is ready you can start either a single instance of Amoc or configure a distributed environment,
for which you should follow the steps described below.

Before running Amoc containers, create a network and start a Graphite instance to collect and visualise some metrics.
```
docker network create amoc-test-network

docker run --rm -d --name=graphite --network amoc-test-network \
    -p 2003:2003 -p 8080:80 graphiteapp/graphite-statsd
```
Start two Amoc containers, export all of the necessary environmental variables so that the nodes can communicate with each other and send metrics to Graphite.
In order to use Amoc HTTP API for uploading and starting scenarios, port 4000 should be published.
```
docker run --rm -t -d --name amoc-1 -h amoc-1 --network ${NETWORK} \
    -e AMOC_HOSTS="\"amoc-1\",\"amoc-2\"" \
    -e AMOC_GRAPHITE_HOST=graphite \
    -e AMOC_GRAPHITE_PORT=2003 \
    -e AMOC_PREFIX=amoc1 \
    --health-cmd="${PATH_TO_AMOC} status" \
    -p 8081:4000 \
    amoc-reworked:latest

docker run --rm -t -d --name amoc-2 -h amoc-2 --network ${NETWORK} \
    -e AMOC_HOSTS="\"amoc-1\",\"amoc-2\"" \
    -e AMOC_GRAPHITE_HOST=graphite \
    -e AMOC_GRAPHITE_PORT=2003 \
    -e AMOC_PREFIX=amoc2 \
    --health-cmd="${PATH_TO_AMOC} status" \
    -p 8082:4000 \
    amoc-reworked:latest
```

Connect to Amoc console and go to the [next](doc/distributed-run.md) section.
```
docker exec -it amoc-1 /home/amoc/amoc/bin/amoc remote_console
