# COMP1100 Assignment 3 - Othello

In this assignment, you will develop an AI that plays [Othello (also
known as Reversi)](https://en.wikipedia.org/wiki/Reversi), a classic
board game. We have implemented the rules of the game for you, but you
will have to decide how best to play the game.

{:.msg-info}
This assignment is worth 15% of your final grade.

{:.msg-warn}
**Deadline**: Sunday 27th October, 2019, at 11:00pm Canberra time *sharp*


## Overview of Tasks

This assignment is marked out of 100:

| **Task**              | **Marks** |
|-----------------------|-----------|
| Main Task: Othello AI | 55 Marks  |
| Unit Tests            | 10 Marks  |
| Style                 | 10 Marks  |
| Technical Report      | 25 Marks  |

{:.msg-warn}
As with assignment 2, code that does not compile will be penalised
heavily. It is **essential** that you can write code that compiles and
runs. If you have a partial solution that you cannot get working, you
should comment it out and write an additional comment directing your
tutor's attention to it.


## Getting Started

Fork the assignment repository and create a project for it in IntelliJ
IDEA, following the same steps as in [Lab
2](https://cs.anu.edu.au/courses/comp1100/labs/02/#forking-a-project). The
assignment repository is at
<https://gitlab.cecs.anu.edu.au/comp1100/comp1100-assignment3>.


## Overview of the Game

Othello (also known as Reversi) is a board game played on an 8x8 board
with 64 discs. Each disc is light on one side and dark on the other.
The game starts with 4 discs in the centre of the board: two dark and
two light.

![Othello starting position](images/starting.png)

To make a move, a player places a disc with their colour facing up on
an unoccupied square on the board, such that it will capture at least
one of the opponents discs.

Capturing occurs if the placed disc forms a line (horizontal, vertical
or diagonal) that has one or more of the opponents discs on it and is
ended by another of the current player's discs, with none of the current
player's discs in-between. All of the opponents
discs on this line are flipped over, becoming the current player's discs.
It is possible (and in fact quite common) for a single move to capture more
than one line, and all the pieces which can be captured in a move must be
captured (flipped).

![Othello capture](images/capture.png)

The dark player plays first, and play alternates between the two
players.  If a player can't make a legal move, their turn is
skipped. If neither player can make a legal move, the game is over and
the player with the most discs wins.


## Overview of the Repository

Most of your code will be written in `src/AI.hs`, but you will also
need to write tests in `test/OthelloTests.hs`. A small program to play
the game (either Human vs. Human, or Human vs. AI, or AI vs. AI) is
provided at `app/Main.hs`, and you can run it with `cabal run`.

`src/GameState.hs` implements the data structures used to implement
the game rules. The game rules themselves are implemented at
`src/Game.hs`. You should read over both these files. While you may
not need to understand every detail of how they work, understanding
_what_ they do will be helpful.

{:.msg-warn}
The only files you are allowed to modify in your submission are
`src/AI.hs` and `test/OthelloTest.hs`. Any other modifications risk
changing the rules of the game, so you would be playing a different
game to the one in the assignment.

{:.msg-warn} For this assignment, GHC is set to be more pedantic than
the previous two assignments. It will treat all warnings as errors,
and you'll only be able to write "Safe" Haskell. If you stick with the
tools we've taught in the course, you shouldn't notice this at all,
with one exception: attempts to use `trace` and similar from
`Debug.Trace` will fail to compile. We have chosen to use Safe Haskell
to prevent your AIs from doing underhanded things in the
tournament.

### Other Files

* `app/Main.hs` implements an interface to play the game, and for you
  to test your skills against your AI. You are not required to
  understand it.

* `comp1100-assignment3.cabal` tells the cabal build tool how to build
  your assignment. You are not required to understand this file, and
  we will discuss how to use cabal below.

* `src/Config.hs` defines a data type used to configure the
  program. You do not need to understand or modify this file.

* The files under `src/Dragons/` contain advanced code that you are
  not required to understand or modify. (On medieval maps they drew
  pictures of dragons or sea monsters over uncharted areas. The code
  in those files is beyond the areas of Haskell which this course
  explores.)

* `test/Testing.hs` is a testing library that is similar to the ones
  used in previous assignments. It has been extended to allow you to
  group related tests together.

* `Setup.hs` tells cabal that this is a normal package with no unusual
  build steps. Some complex packages (that we will not see in this
  course) need to put more complex code here. You are not required to
  understand it.


## Overview of Cabal

As before, we are using the `cabal` tool to build the assignment
code. The commands provided are very similar to last time:

* `cabal build`: Compile your assignment.

* `cabal run othello`: Build your assignment (if necessary), and run
  on the test program. We discuss the test program in detail below,
  as there are a number of ways to launch it.

* `cabal repl comp1100-assignment3`: Run the GHCi interpreter over
  your project.

* `cabal test`: Build and run the tests. This assignment is set up to
   run a unit test suite, and as with Assignment 2 you will be writing
   tests. The unit tests will abort on the first failure, or the first
   call to a function that is `undefined`.

{:.msg-info}
You should execute these cabal commands in the **top-level directory** of your
project: `~/comp1100/assignment3` (i.e., the directory you are in when you
launch the IntelliJ Terminal tool for your project).


## Overview of the Test Program

When you run `cabal run othello`, you play the game against your AI
called "default". This is done through a web browser, in the same way
as previous CodeWorld assignments.

You can change the behavior of the program by calling it with
different arguments, like this: `cabal run othello -- arg1 arg2
arg3...`. These are the arguments you can provide:

| **Argument**    | **Effect**                                                              |
|-----------------|-------------------------------------------------------------------------|
| `-T GAMETYPE`   | Choose how to run the game: `console`/`gui`.                            |
| `-t TIMEOUT`    | How much time to give the AI to make a move.                            |
| `-p PLAYER1`    | Choose the AI name for the dark player, or `HUMAN` to play as a human. |
| `-P PLAYER2`    | Choose the AI name for the light player, or `HUMAN` to play as a human.  |
| `-H HOSTNAME`   | Name of the computer to connect to for a network game.                  |
| `-n PORTNUMBER` | Port number to host/connect on for a network game.                      |
| `-h`            | Print help text and exit.                                               |

Example: `cabal run othello -- -t 0.1 -p default` will play the
default AI against itself, and give it 0.1 seconds to make a move.
The default AI that you get when you clone the repository plays the
first legal move it can find, so playing it against itself should
always look like this:

![First legal move game](images/first-legal-game.png)


## Main Task: Othello AI (55 Marks)

### Your Task

Implement an AI (of type `AI`) for Othello in `src/AI.hs`. There is a
list called `ais` in that file, and we will mark the AI you call
"default" in that list.

We will test your AI's performance by comparing it to implementations
written by course staff, using a variety of standard approaches. Its
performance against these AIs will form a large part of the marks for
this Task.

{:.msg-warn}
It is **vital** that you indicate one AI as "default", otherwise we will
not know which one to mark.

### Understanding the `AI` Type

The `AI` type is an alias for `Game -> Int -> Position`. The `Game`
argument describes the current state of the game, and the `Int`
argument is an indication of how far you might want to look ahead when
searching for a good move.

You do not have to arrange for your AI to be called; any test program
we provide will do so for you. When it is your AI's turn, we will call
your AI with the current game state and lookahead `1`, then `2`, then
`3`, etc, until four seconds have passed overall. The most recent
result will be taken as the final result.

{:.msg-info}
While you are testing, you may want to use the `-t` option to set a
shorter timeout, so that you can test changes more quickly.

Very simple AIs that do not look ahead will ignore the `Int` argument.

### Discussion

Your AI should inspect the `Turn` within the `Game` to see whose turn
it is. You may call `error` if the `Turn` is `GameOver` - your AI
should never be called on a finished game. Your AI can then use the
`Player` value and `opponent` function to work out how to evaluate the
board.

{:.msg-info}

You may also assume that we will only ever call your AI if there is a
legal move it can make. In particular, this means that we will not
deduct marks for assuming that a list of legal moves is non-empty
(e.g., you used the `head` function). Note that gratuitous use of
`head` and `tail` is still poor style.

This is a very open-ended task, and it will probably help if you build
up your solution a little at a time. We suggest some approaches below.

### First Legal Move

The simplest AI you can build is one that makes the first legal move
it can. We have provided this for you, so you can see what a simple
`AI` looks like.

### Interlude: Heuristics

Heuristic functions were discussed in [the lecture on game
trees](https://cs.anu.edu.au/courses/comp1100/lectures/09-1-Game_Trees.pdf). We
expect the quality of your heuristic function - how accurately it
scores game states - to have a large impact on how well your AI
performs.

### Greedy Strategy

"Greedy strategies" are the class of strategies that make moves that
provide the greatest _immediate_ advantage. In the context of this
game, it means always making the move that will give it the greatest
increase in heuristic. Try writing a simple heuristic and a greedy
strategy, and see whether it beats your "first legal move" AI.

### Interlude: Game Trees

To make your AI smarter, it is a good idea for it to look into the
future and consider responses to its moves, its responses to those
responses, and so on. The lecture on game trees may help you here.

{:.msg-warn}
It is possible for one player to make two moves in a row, if the
opponent has no legal move. Therefore, it is not safe to assume that
each layer of the game tree is scored for opposing players.

### Minimax

Greedy strategies can often miss opportunities that need some
planning, and get tricked into silly traps by smarter opponents. The
Minimax Algorithm was discussed in the lecture on game trees and will
likely give better performance than a greedy strategy.

### Pruning

Once you have Minimax working, you may find that your AI exploring a
number of options that cannot possibly influence the result. Cutting
off branches of the search space early is called _pruning_, and one
effective method of pruning is called [Alpha-Beta
Pruning](https://cs.anu.edu.au/courses/comp1100/lectures/09-2-Alpha_Beta.pdf),
which was discussed in lectures. Good pruning may allow your search to
explore deeper within the time limit it has to make its move.

### Other Hints

* There are four main ways your AI can be made smarter:

  - Lookahead: If your function runs efficiently, it can see further
    into the future before it runs out of time. The more moves into
    the future it looks, the more likely it will find good moves that
    are not immediately obvious. Example: at 1 level of lookahead, a
    move may let you capture a lot of dics, but at deeper lookahead
    you might see that it leaves you open to a large counter-capture.

  - Heuristic: You will not have time to look all the way to the end
    of every possible game. Your heuristic function guesses how good a
    `Game` is for each player. If your heuristic is accurate, it will
    correctly identify strong and weak states.

  - Search Strategy: This determines how your AI decides which
    heuristic state to aim for. Greedy strategies look for the best
    state they can (according to the heuristic) and move towards that
    state. More sophisticated strategies like Minimax consider the
    opponent's moves when planning.

  - Pruning: if you can discard parts of the game tree without
    considering them in detail, you can process game trees faster and
    acheive a deeper lookahead in the allotted running
    time. Alpha-beta pruning is one example; there are others.

* Choosing a good heuristic function is very important, as it gives
  your AI a way to value its position that is smarter than just
  looking at current score. Perhaps you might find that some squares
  are more valuable than others, when it comes to winning games, and
  so your AI should value them more highly.

* Do not try to do everything at once. This does not work in production
  code and often does not work in assignment code either. Get something
  working, then take your improved understanding of the problem to the
  more complex algorithms.

* As you refine your AIs, test them against each other to see whether
  your changes are actually an improvement.


## Unit Tests (10 Marks)

As with Assignment 2, you will be expected to write unit tests to
convince yourself that your code is correct. The testing code has been
extended from last time - `test/Testing.hs` now allows you to group
tests into a tree structure. As before, you run the tests using `cabal
test`.

### Your Task

Add tests to `test/OthelloTest.hs` that test your AI.

#### Hints

* Most of the hints from Assignment 2 apply here. Reread those.

* If a function is giving you an unexpected result, try breaking it
  into parts and writing tests for each part. This helps you isolate
  the incorrect parts, and gives you smaller functions to fix.

* If your function has subtle details that need to be correct, think
  about writing tests to ensure those details do not get missed as you
  work on your code.


## Style (10 Marks)

As you write increasingly complex code, it is increasingly important
that the code remains readable. This saves wasted effort understanding
messy code, which makes it easier to think about the problem and your
solution to it.

### Your Task

Ensure that your code is written in good Haskell style.


## Technical Report (25 marks)

You should write a concise [technical
report](../../resources/06-reports). An excellent report will:
demonstrate conceptual understanding of all major functions, and how
they interact when the program as a whole runs; explain your design
process, including your assumptions, and the reasons behind choices
you made; discuss how you tested your program, and in particular why
your tests give you confidence that your code is correct; and be well
formatted without spelling or grammar errors.

The **maximum word count is 1500**. This is a *limit*, not a *quota*;
concise presentation is a virtue.

{:.msg-warn}
Once again: This is not a required word count. They are the **maximum
number of words** that your marker will read. If you can do it in
fewer words without compromising the presentation, please do so.

Your report must be in PDF format, located at the root of your
assignment repository on GitLab and named `Report.pdf`. Otherwise, it
may not be marked.

The report must have a **title page** with the following items:

* Your name
* Your laboratory time and tutor
* Your university ID

An excellent report will:
  * demonstrate conceptual understanding of all major
    functions, and how they interact when the program as a whole runs;
  * explain your design process, including your assumptions, and the reasons
    behind choices you made;
  * discuss how you tested your program, and in particular why your tests give
    you confidence that your code is correct; and
  * be well formatted without spelling or grammar errors.

### Content and Structure

Your audience is the tutors and lecturers, who are proficient at programming
and understand most concepts. Therefore you should not, for example, waste
words describing the syntax of Haskell or how recursion works. After reading
your technical report, the reader should thoroughly understand what problem
your program is trying to solve, the reasons behind major design choices in it,
as well as how it was tested. Your report should give a broad overview of your
program, but focus on the specifics of what *you* did and why.

Remember that the tutors have access to the above assignment
specification, and if your report *only* contains details from it then
you will only receive minimal marks. Below is an potential outline for
the structure of your report and some things you might discuss in it.

#### Introduction

If you wish to do so you can write an introduction. In it, give:

* A brief overview of your program:

  - how it works; and
  - what it is designed to do.

#### Content

Talk about why you structured the program the way you did. Below are some
questions you could answer:

* Program design
  - Describe what each relevant function does conceptually. (i.e. how
    does it get you closer to solving the problems outlined in this
    assignment spec?)
  - How do these functions piece together to make the finished
    program? Why did you design and implement it this way?
  - What major design choices did *you* make regarding the functions
    that *you’ve* written, and the overall structure of your program?

  - For this assignment specifically, you could also ask yourself:
    - How does your AI select a good move?
    - What data structures did you choose, and why?
    - How did you develop the AI that is your main submission?

* Assumptions
  - Describe assumptions you have made
    and how this has influenced your design decisions.

* Testing
  - How did you test individual functions?
    - Be specific about this - the tutors know that you have tested
      your program, but they want to know *how*.
    - Describe the tests that prove individual functions on their own
      behave as expected (e.g. testing a function with different
      inputs and doing a calculation by hand to check that the outputs
      are correct).
  - How did you test the entire program? What tests did you perform to
    show that the program behaves as expected in all (even unexpected)
    cases?

* Inspiration / external content
  - What resources did you use when writing your program (e.g.,
    published algorithms)?
  - If you have used resources such as a webpage describing an
    algorithm, be sure to cite it properly at the end of your report
    in a ‘References’ section. References do not count to the maximum
    word limit.

#### Reflection

Discuss the reasoning behind your decisions, rather than *what* the
decisions were. You can reflect on not only the decisions you made,
but the process through which you developed the final program:

* Did you encounter any conceptual or technical issues?
  - If you solved them, describe the relevant details of what happened
    and how you overcame them.
  - Sometimes limitations on time or technical skills can limit how
    much of the assignment can be completed. If you ran into a problem
    that you could not solve, then your report is the perfect place to
    describe it. You could include details such as:

    - Theories as to what caused the problem;
    - Suggestions of things that might have fixed it; and
    - Discussion about what you did try, and the results of these attempts.

* What would you have done differently if you were to do it again?
  - What changes to the design and structure you would make if you
    wrote the program again from scratch?

* Are parts of the program confusing for the reader? You can explain
  them in the report (in this situation you should also make use of
  comments in your code).

* If you collaborated with others, what was the nature of the
  collaboration?  (Note that you are only allowed to collaborate by
  sharing ideas, not code.)
  - Collaborating is any discussion or work done together on planning
    or writing your assignment.

* Other info
  - You may like to briefly discuss details of events which were
    relevant to your process of design - strange or interesting things
    that you noticed and fixed along the way.

{:.msg-info}
This is a list of **suggestions**, not requirements. You should only
discuss items from this list if you have something interesting to
write.

### Things to avoid in a technical report

* Line by line explanations of large portions of code. (If you want to
  include a specific line of code, be sure to format as described in
  the "Format" section below.)
* Pictures of code or IntelliJ.
* Content that is not your own, unless cited.
* Grammatical errors or misspellings. Proof-read it before submission.
* Informal language - a technical report is a professional document, and as
  such should avoid things such as:
  - Unnecessary abbreviations (atm, btw, ps, and so on), emojis, and
    emoticons; and
  - Stories / recounts of events not relevant to the development of the program.
* Irrelevant diagrams, graphs, and charts. Unnecessary elements will
  distract from the important content. Keep it succinct and focused.

If you need additional help with report writing, the [academic skills
writing
centre](http://www.anu.edu.au/students/academic-skills/appointments/academic-skills-writing-centre)
has a peer writing service and writing coaches.


### Format

You are not required to follow any specific style guide (such as APA
or Harvard). However, here are some tips which will make your report
more pleasant to read, and make more sense to someone with a computer
science background.

* Colours should be kept minimal. If you need to use colour, make sure it is
  absolutely necessary.
* If you are using graphics, make sure they are *vector* graphics (that stay
  sharp even as the reader zooms in on them).
* Any code, including type/function/module names or file names, that
  appears in your document should have a monospaced font (such as
  Consolas, Courier New, Lucida Console, or Monaco)
* Other text should be set in serif fonts (popular choices are Times,
  Palatino, Sabon, Minion, or Caslon).
* When available, automatic *ligatures* should be activated.
* Do not use underscore to highlight your text.
* Text should be at least 1.5 spaced.


## Communication

**Do not** post your code publicly, either on Piazza or via other
forums. Posts on Piazza trigger emails to all students, so if by
mistake you post your code publicly, others will have access to your
code and you may be held responsible for plagiarism.

Once again, and we cannot stress this enough: **do not post your code
publicly** . If you need help with your code, post it *privately* to the
instructors.

When brainstorming with your friends, **do not share code**. There
might be pressure from your friends, but this is for both your and
their benefit. Anything that smells of plagiarism will be investigated
and there may be serious consequences.

Sharing ideas and sketches is perfectly fine, but sharing should stop
at ideas.

Course staff will not look at assignment code unless it is posted
**privately** in piazza.

Course staff will typically give assistance by asking questions,
directing you to relevant exercises from the labs, or definitions and
examples from the lectures.

{:.msg-info}
Before the assignment is due, course staff will not give individual
tips on writing functions for the assignment or how your code can be
improved. We will help you get unstuck by asking questions and
pointing you to relevant lecture and lab material. You will receive
feedback on your work when marks are released.


## Submission Advice

Start early, and aim to finish the assignment several days before the
due date. At least 24 hours before the deadline, you should:

* Re-read the specification one final time, and make sure you've
  covered everything.

* Confirm that the latest version of your code has been pushed to
  GitLab.

* Ensure your program compiles and runs, including the `cabal test`
  test suite.

* Ensure your submission works on the lab machines. If it does not, it
  may fail tests used by the instructors.

* Proof-read and spell-check your report.

* Verify that your report is in PDF format, in the root of the project
  directory (not in `src`), and named `Report.pdf`. That capital `R`
  is important - Linux uses a case-sensitive file system. Check that
  you have succesfully added it in GitLab.
# comp1100 assignment3 Haskell AI Othello
# 加微信 powcoder

# [代做各类CS相关课程和程序语言](https://powcoder.com/)

# Programming Help Add Wechat powcoder

[成功案例](https://powcoder.com/tag/成功案例/)

[java代写](https://powcoder.com/tag/java/) [c/c++代写](https://powcoder.com/tag/c/) [python代写](https://powcoder.com/tag/python/) [drracket代写](https://powcoder.com/tag/drracket/) [MIPS汇编代写](https://powcoder.com/tag/MIPS/) [matlab代写](https://powcoder.com/tag/matlab/) [R语言代写](https://powcoder.com/tag/r/) [javascript代写](https://powcoder.com/tag/javascript/)

[prolog代写](https://powcoder.com/tag/prolog/) [haskell代写](https://powcoder.com/tag/haskell/) [processing代写](https://powcoder.com/tag/processing/) [ruby代写](https://powcoder.com/tag/ruby/) [scheme代写](https://powcoder.com/tag/drracket/) [ocaml代写](https://powcoder.com/tag/ocaml/) [lisp代写](https://powcoder.com/tag/lisp/)

- [数据结构算法 data structure algorithm 代写](https://powcoder.com/category/data-structure-algorithm/)
- [计算机网络 套接字编程 computer network socket programming 代写](https://powcoder.com/category/network-socket/)
- [数据库 DB Database SQL 代写](https://powcoder.com/category/database-db-sql/)
- [机器学习 machine learning 代写](https://powcoder.com/category/machine-learning/)
- [编译器原理 Compiler 代写](https://powcoder.com/category/compiler/)
- [操作系统OS(Operating System) 代写](https://powcoder.com/category/操作系统osoperating-system/)
- [计算机图形学 Computer Graphics opengl webgl 代写](https://powcoder.com/category/computer-graphics-opengl-webgl/)
- [人工智能 AI Artificial Intelligence 代写](https://powcoder.com/category/人工智能-ai-artificial-intelligence/)
- [大数据 Hadoop Map Reduce Spark HBase 代写](https://powcoder.com/category/hadoop-map-reduce-spark-hbase/)
- [系统编程 System programming 代写](https://powcoder.com/category/sys-programming/)
- [网页应用 Web Application 代写](https://powcoder.com/category/web/)
- [自然语言处理 NLP natural language processing 代写](https://powcoder.com/category/nlp/)
- [计算机体系结构 Computer Architecture 代写](https://powcoder.com/category/computer-architecture/)
- [计算机安全密码学computer security cryptography 代写](https://powcoder.com/category/computer-security/)
- [计算机理论 Computation Theory 代写](https://powcoder.com/category/computation-theory/)
- [计算机视觉(Compute Vision) 代写](https://powcoder.com/category/计算机视觉compute-vision/)

