
all:
	xsltproc --stringparam library-version `perl -ne 'print "$$1\n" if (/:version "(.*)"/)' ../drakma.asd` clixdoc.xsl index.xml > index.html
