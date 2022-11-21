logs ?=false
method ?=get-tag

options := --vus 20 --duration 60s
k6-run := k6 run $(options)

run:
	$(k6-run) src/${method}.js --env LOGS=$(logs)

