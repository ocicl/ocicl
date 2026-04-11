DOCKER_USER=binghelisp
DOCKER_IMAGE=$(DOCKER_USER)/lisp-dev

build:
	docker buildx build --platform linux/i386,linux/amd64,linux/arm64/v8 -t $(DOCKER_IMAGE) .

push:
	docker push $(DOCKER_IMAGE)

run-i386:
	docker pull --platform linux/i386 $(DOCKER_IMAGE)
	docker run --platform linux/i386 -ti \
		-v Lisp:/Lisp -v ~/Downloads:/hgfs $(DOCKER_IMAGE)

run-amd64:
	docker pull --platform linux/amd64 $(DOCKER_IMAGE)
	docker run --platform linux/amd64 -ti \
		-v Lisp:/Lisp -v ~/Downloads:/hgfs $(DOCKER_IMAGE)

run-arm64:
	docker pull --platform linux/arm64/v8 $(DOCKER_IMAGE)
	docker run --platform linux/arm64/v8 -ti \
		-v Lisp:/Lisp -v ~/Downloads:/hgfs $(DOCKER_IMAGE)

run:
	echo "use make run-i386, run-amd64 or run-arm64"
