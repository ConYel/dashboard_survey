# dashboard_survey
![alt text](https://github.com/ConYel/dashboard_survey/blob/main/STAIRCASE.png?raw=true)
## Get repository and build image
```
git clone https://github.com/ConYel/dashboard_survey.git
cd dashboard_survey
docker build --tag dash_survey:v0.2
```

to test the rscript run:
(considering that you are still in the same dir of the repository)
```r
docker run --rm -P  -v ./testdt.txt:/home/testdt.txt  dash_survey:v0.2 /home/testdt.txt

```

In another terminal check the port exposed by docker:
```
docker ps -a
```
PORTS
0.0.0.0:33768->3131/tcp

Then check the shiny app running on your browser using:
localhost:33768


If the above doesn't work you can close the previous docker and run:

```
docker run --rm -p 3131:3131  -v ./testdt.txt:/home/testdt.txt  dash_survey:v0.2 /home/testdt.txt
```
Then check the shiny app running on your browser using:
localhost:3131


If you want to know more how the option `-p 3131:3131` works please read the documentation:
https://docs.docker.com/engine/reference/run/#expose-incoming-ports
```
--expose=[]: Expose a port or a range of ports inside the container.
             These are additional to those exposed by the `EXPOSE` instruction
-P         : Publish all exposed ports to the host interfaces
-p=[]      : Publish a container's port or a range of ports to the host
               format: ip:hostPort:containerPort | ip::containerPort | hostPort:containerPort | containerPort
               Both hostPort and containerPort can be specified as a
               range of ports. When specifying ranges for both, the
               number of container ports in the range must match the
               number of host ports in the range, for example:
                   -p 1234-1236:1234-1236/tcp

               When specifying a range for hostPort only, the
               containerPort must not be a range.  In this case the
               container port is published somewhere within the
               specified hostPort range. (e.g., `-p 1234-1236:1234/tcp`)

               (use 'docker port' to see the actual mapping)
```
