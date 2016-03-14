# 8Putthles

Introduction

  The 8-puzzle is a well-known “toy” problem in AI, often used to illustrate search concepts. To
solve the puzzle, you must slide 8 tiles about a 3x3 grid to reach a goal state. The 8-puzzle
provides an excellent introduction to state space search.

Problem

  Write a Lisp program to solve the 8-puzzle using BFS (breadth first search), DFID (depth first
iterated deepening), and A*. The start position may be specified in a puzzle file, in the Lisp
function call, or interactively. After solving the puzzle with each search algorithm, print out a
nicely formatted list of positions, leading from the start state to the goal state. Also print out the
number of moves required to reach the goal state, and the number of nodes generated and
expanded (i.e., the number placed on the OPEN and CLOSED lists, respectively). This will
provide a rough metric for search algorithm efficiency.

Implementation

  Your Lisp program (8puzzle.lsp) may be run in three different ways:
Command-line usage: clisp 8puzzle.lsp puzzlefile
The puzzlefile contains an 8-puzzle start position, consisting of 9 digits separated by white space,
in row-major order. The digits 1-8 represent the 8 tiles, and 0 represents the blank.
Inside CLISP: (load '8puzzle)
(8puzzle [puzzlelist])
  The optional puzzlelist argument contains an 8-puzzle start position, stored in row-major order as
a list of 9 elements. The digits 1-8 represent the 8 tiles, and 0 represents the blank. If the
puzzlelist argument is not supplied, prompt the used to enter the start position as 9 digits
separated by white space (not as a list).

Search algorithms

  BFS and DFID are fairly self-explanatory. Implement at least two admissible and one
inadmissible A* heuristics that are designed for the 8-puzzle (e.g., number of tiles out of place).
More informed heuristics that search more efficiently will be awarded greater credit.
Format your solution output as shown in the sample session below. You should display several
problem states across the page (not just vertically).
Extra credit
Generalize the 8-puzzle to the N-puzzle, where N may be 32
-1=8, 42
-1=15,
5
2
-1=24, etc. You may need to add the puzzle size to the command-line arguments and puzzle
file format.
