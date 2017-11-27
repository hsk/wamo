WAMCC=../wamo
SOURCES=$(shell ls *.pl)
WAMFILES= $(SOURCES:.pl=.wam)

all: $(WAMFILES)

%.wam : %.pl
	$(WAMCC) -i $< -v > $@

clean:
	-rm -f $(WAMFILES)

stats:
	ls $(WAMFILES) | xargs wc -l
