PKG = klotski
VERSION = 0.1
DIST = $(PKG)-$(VERSION)

dist:
	rm -rf $(DIST)
	mkdir $(DIST)
	cp *.el $(DIST)
	cp README.org $(DIST)/README
	sed -e 's/KLOTSKI_VERSION/$(VERSION)/' < klotski-pkg.el > \
		$(DIST)/klotski-pkg.el
	tar -cf $(DIST).tar $(DIST)

clean:
	rm -rf $(DIST)
	rm -f $(DIST).tar
	rm -f *.elc
