test:
	sbt 'run --backend c --compile --test --genHarness'

clean:
	rm -f *.cpp *.h *.o

.PHONY: test clean
