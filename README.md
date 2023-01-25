# Latin-squares-Haskell
It is a program that finds for input N (belonging to natural numbers) all reduced Latin squares of size N (and their number), and the set of all mutually orthogonal Latin squares of size N (also with their number).

function call: latinsq N file1 file2

Saves all reduced Latin squares of N × N dimension (and their number) to the file file1 - as a tuple (reduced Latin squares, number of these squares).
To file2 writes all mutually orthogonal squares of dimension N × N (and their number) with the first row equal to [1,2,...,N] (similar to file1 - as a tuple).


Matrix is represented as a list of rows (which are also lists), e.g.:

[1,2,3,4,5]\
[2,3,4,5,1]\
[3,4,5,1,2]\
[4,5,1,2,3]\
[5,1,2,3,4]

is represented as:

[[1,2,3,4,5],[2,3,4,5,1],[3,4,5,1,2],[4,5,1,2,3],[5,1,2,3,4]]


example inputs:
latinsq 3 "exampleFile1.txt" "exampleFile2.txt"
latinsq 4 "exampleFile1.txt" "exampleFile2.txt"
latinsq 5 "exampleFile1.txt" "exampleFile2.txt"
