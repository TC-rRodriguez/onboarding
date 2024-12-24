build:
	docker build -t api .

run:
	docker run -it --rm -p 8080:8080 api

test:
	docker run -it --rm api rebar3 ct

clean:
	docker rmi api