# dashboard_survey

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

In another terminal check the port give by docker:
```
docker ps -a
```
PORTS
0.0.0.0:33768->3131/tcp

Then check the shiny app running on your browser using:
localhost:33768
