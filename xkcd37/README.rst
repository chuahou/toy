#################
xkcd 37 (Haskell)
#################

A simple function ``xkcdify`` that performs something best described by
the `original xkcd 37 <https://xkcd.com/37/>`_.

.. image:: https://imgs.xkcd.com/comics/hyphen.jpg
	:width: 400
	:alt: xkcd 37

Examples
========

::

	λ> xkcdify "Man, that's a sweetass car."
	"Man, that's a sweet ass-car."
	λ> xkcdify "Man, that's a sweet-ass car."
	"Man, that's a sweet ass-car."
