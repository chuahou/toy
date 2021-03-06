######################
Bogobogosort (Haskell)
######################

This is an implementation of the best sorting algorithm I've encountered
so far,
`Bogobogosort <https://www.dangermouse.net/esoteric/bogobogosort.html>`_.

Results
=======

The test function ``testbbs`` was run on lists of lengths 1 to 10 for 5
times each. The timed results are:

+--------+----------+
| Length | Time (s) |
+========+==========+
| 1      | 0.01     |
+--------+----------+
| 2      | 0.01     |
+--------+----------+
| 3      | 0.01     |
+--------+----------+
| 4      | 0.01     |
+--------+----------+
| 5      | 0.02     |
+--------+----------+
| 6      | 0.04     |
+--------+----------+
| 7      | 0.16     |
+--------+----------+
| 8      | 2.47     |
+--------+----------+
| 9      | 21.40    |
+--------+----------+
| 10     | 85.75    |
+--------+----------+

::

	λ> :r
	[1 of 1] Compiling Main             ( Bogobogosort.hs, interpreted )
	Ok, one module loaded.
	λ> :set +s
	λ> testbbs 5 [1..1]
	[1]
	[1]
	[1]
	[1]
	[1]
	(0.01 secs, 105,384 bytes)
	λ> testbbs 5 [1..2]
	[1,2]
	[1,2]
	[1,2]
	[1,2]
	[1,2]
	(0.01 secs, 203,360 bytes)
	λ> testbbs 5 [1..3]
	[1,2,3]
	[1,2,3]
	[1,2,3]
	[1,2,3]
	[1,2,3]
	(0.01 secs, 408,400 bytes)
	λ> testbbs 5 [1..4]
	[1,2,3,4]
	[1,2,3,4]
	[1,2,3,4]
	[1,2,3,4]
	[1,2,3,4]
	(0.01 secs, 940,392 bytes)
	λ> testbbs 5 [1..5]
	[1,2,3,4,5]
	[1,2,3,4,5]
	[1,2,3,4,5]
	[1,2,3,4,5]
	[1,2,3,4,5]
	(0.02 secs, 4,271,408 bytes)
	λ> testbbs 5 [1..6]
	[1,2,3,4,5,6]
	[1,2,3,4,5,6]
	[1,2,3,4,5,6]
	[1,2,3,4,5,6]
	[1,2,3,4,5,6]
	(0.04 secs, 22,586,512 bytes)
	λ> testbbs 5 [1..7]
	[1,2,3,4,5,6,7]
	[1,2,3,4,5,6,7]
	[1,2,3,4,5,6,7]
	[1,2,3,4,5,6,7]
	[1,2,3,4,5,6,7]
	(0.16 secs, 153,380,472 bytes)
	λ> testbbs 5 [1..8]
	[1,2,3,4,5,6,7,8]
	[1,2,3,4,5,6,7,8]
	[1,2,3,4,5,6,7,8]
	[1,2,3,4,5,6,7,8]
	[1,2,3,4,5,6,7,8]
	(2.47 secs, 3,234,910,464 bytes)
	λ> testbbs 5 [1..9]
	[1,2,3,4,5,6,7,8,9]
	[1,2,3,4,5,6,7,8,9]
	[1,2,3,4,5,6,7,8,9]
	[1,2,3,4,5,6,7,8,9]
	[1,2,3,4,5,6,7,8,9]
	(21.40 secs, 28,542,473,688 bytes)
	λ> testbbs 5 [1..10]
	[1,2,3,4,5,6,7,8,9,10]
	[1,2,3,4,5,6,7,8,9,10]
	[1,2,3,4,5,6,7,8,9,10]
	[1,2,3,4,5,6,7,8,9,10]
	[1,2,3,4,5,6,7,8,9,10]
	(85.75 secs, 149,945,559,944 bytes)
	λ>
