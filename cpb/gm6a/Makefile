gm6a : gm6a.go character.go
	go build gm6a.go character.go 

run :
	go run character.go gm6a.go kids-pcs-3.json

test :
	go test

tar :
	rm -f gm6a.tar.gz
	tar cvf gm6a.tar --exclude OLD *
	gzip -9v gm6a.tar

clean :
	-rm gm6a
