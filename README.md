FUEL = FUnctional Evolutionary aLgorithms
===================================================

Krzysztof (Chris) Krawiec, <krawiec at cs.put.poznan.pl>
June 2014 - Nov 2015

Introduction
===================

FUEL is a succinct Scala framework for implementing metaheuristic algorithms, in particular evolutionary algorithms. It originated in my work on the book "Behavioral Program Synthesis with Genetic Programming" (Springer 2016, <http://www.cs.put.poznan.pl/kkrawiec/bps/>, <http://www.springer.com/gp/book/9783319275635>)

FUEL is written primarily in functional style, with most classes implemented as immutable, and using object-oriented style rather sparingly. 

Features: 
- Simple and lightweight (around 2000 lines of code, including several examples of usage)
- Easy manipulation of components (e.g., hybridizing search algorithms, search operators, etc.)
- Most components implemented as immutable
- Applicable to single- and multiobjective problems
- Natural support for parallelization
- Easily interoperable with Java
- No dependencies on external libraries
- Ready-to-use operators for solutions represented as vectors and permutations

FUEL can be particularly useful for using metaheuristics in innovative ways, e.g., for hybridizing them with other algorithms or devising 'homebrew' algorithms. 

What follows is a short description of the basic components and the relations between them. More detailed explanations are provided in source code as comments. 


Basic concepts
===================

A metaheuristic algorithm is a compound function. FUEL's role is to:

* help building an algorithm from the components available in the library, 
* provide convenient ways of parameterizing the algorithms, and 
* facilitate running algorithms and collecting results. 

FUEL offers a range components, most of them stateless. A metaheuristics is an iterative algorithm that iterates over states. A single step of such iteration is a function `State => State`. The object Iteration constructs an iterative algorithm (a function `State => State`) with a given step function (also a function `State => State`) and stopping/termination condition(s) (functions `State => Boolean`). 

A State can be basically anything (and because of that we do not define such a trait); it will typically hold a candidate solution or a population of candidate solutions. For the latter case, we provide `StatePop` trait and `Population` class; population holds a *list* of solutions, so duplicates are permitted. If there is need of storing some form of history of search process (like e.g. in Tabu search), this should also be done in State.  

Solutions can be anything, so they are often parameters of generic types/classes, typically denoted by S (though S is also sometimes used for `State`). For instance, `Population[Vector[Int]]` is a population of solutions being vectors of `Int`s. 

A solution can undergo an evaluation, the outcome of which can be anything (though in practice evaluations are usually `Double`s, `Int`s, in general types implementing complete orders). Evaluation type is another parameter of many generics in FUEL, typically denoted by `E`. We often need to store solutions with their evaluations; the convention we use for that purpose is Scala's `Tuple2` type. So for instance `Population[Tuple2[Seq[Int],Double]]` (or more succinctly `Population[(Seq[Int],Double)]`) is a population of sequences of Ints evaluated by Doubles. Multiobjective evaluation can be realized by defining evaluation as, e.g., a `Seq[Double]`, and using an instance of Dominance trait (a kind of partial order). 

New solutions are built from the existing ones using search operators, i.e. in general functions of signature `S x ... x S => S x ... x S`. For instance a typical mutation operator has the signature `S => S`. Such an operator is 'wrapped' using the `SearchOperator` trait. This trait is extended with convenience classes `SearchOperator1`, `SearchOperator2`, etc. for operators of different arity. A search operator is allowed to fail, in which case it returns an empty list of solutions. 

In order to employ multiple search operators alongside each other, use an instance of `Moves` trait to keep them together and present them to a search algorithm. A class that extends `Moves` has to implement also a method for creating new solutions. The package fuel.moves provides basic implementations of moves for most popular solution representations: vectors of bits, vector of real numbers and for permutations. See `moves.VectorMoves` for an example.  

To use `Moves` in an iterative search, you should use `Breeder`. A breeder uses a selection operator (`Selection`) to select parent solutions from the current population, applies a search operator from `Moves`, obtaining so children solutions, and then checks the children solutions for feasibility, rejecting the infeasible ones. A breeder repeats these steps until sufficiently many children are bred - in the case of the default `SimpleBreeder`, when the population of children reaches the size of parents' population. `SimpleBreeder` applies the particular search operators from `Moves` according to the given probability distribution. 

The above components can be built either by calling constructors (or static methods in companion objects). The abstract class `EACore` provides the means for constructing basic evolutionary algorithms. Class `SimpleEA` extends it with simple reporting and default selection and breeding mechanisms (`TournamentSelection` and `SimpleBreeder`).  


Setup and reporting
-------------------

Other important components of FUEL are:
* `Options` (provides options to set components' parameters), and
* a `Collector` (offers the functionality to store experiment results). 
* a random number generator (an instance of `TRandom`). 

Use OptCollRng to obtain a triple of these objects by parsing a parameter string, or an Array of strings, for instance passed from the command line (see `example.MaxOnesVectors`). An even more succinct way is to implement the FApp trait that does it for you (see `example.MaxOnes`). 

The default `Collector` stores the results in a text file in the Java properties format. By default, the file is given a random unique name. To change its name and location use `--outFile` and `--outDir` options. 


Running a search process
----------------

The algorithm obtained by calling `SimpleAE.algorithm` is a regular function that can be launched individual. However, it may be worthy running it within an `Experiment`. The `Experiment` will run the `Algorithm` and safeguard against exceptions, reporting them (and a few other things like timing) via a `Collector`. Use `--printResults` to print the results collected by `Collector` to standard output (and not only to file). 

The package `fuel.example` presents several usage scenarios for discrete, continuous, and combinatorial optimization. 
The recommended order looking at them is: `MaxOnes`, `MaxOnesVectors`, `Hiff`, `Rosenbrock`, `TSP`, `TSPMultiobjective`. Simple genetic algorithm on bitstrings can be configured and run like this:

~~~~~{.scala}
object MaxOnes2 extends IApp('numVars -> 500, 'maxGenerations -> 200,
  'printResults -> true) {
  RunExperiment(SimpleEA(
    moves = BitSetMoves(opt('numVars, (_: Int) > 0)),
    eval = (s: BitSet) => s.size,
    optimalValue = 0))
}
~~~~~


Package organization
--------------------

* `fuel.core`: Elementary concepts: `Population`, `Dominance`
* `fuel.func`: Main components: `Evaluation`, `Selection`, `Breeding`, `Algorithm`, etc. 
* `fuel.example`: Ready-to-run examples (runnable as independent programs or from Scala REPL; see illustration in `MaxOnes`). 
* `fuel.moves`: Definitions of basic search operators for a few domains. 
* `fuel.util`: Helper objects and functions


Technical comments
--------------------

For convenience, the components (particularly the top-level ones) often use implicit arguments (in particular for `Options`, `Collector` and `RNG`). This immensely reduces the number of parameters that need to be passed when setting up an algorithm/system. Note that, to be used in that mode, a value has to be declared with the 'implicit' keyword. Obviously, such arguments can be alternatively passed explicitly. 

The randomized operations rely on `util.TRandom`, which is intended to serve as a wrapper for `java.util.Random`, or any other (possibly better) random number generator of your choice. 
 
FUEL uses assertions to dynamically check invariants. Assertions can be disabled by passing the `-Xdisable-assertions` argument to Scala compiler, which may result in some performance improvement. 


How to cite 
===================

If you decide to use FUEL and like it, please cite my book, *Behavioral Program Synthesis with Genetic Programming*  published by Springer. You can find out more about the book [here](http://www.cs.put.poznan.pl/kkrawiec/bps/). 

~~~~~{.bib}
@book{KrawiecBPS2015,
    title = {Behavioral Program Synthesis with Genetic Programming},
    author = {Krzysztof Krawiec},
    publisher = {Springer International Publishing},
    series = {Studies in Computational Intelligence},
    year = 2016,
    volume = {618},
    doi = {10.1007/978-3-319-27565-9},
    isbn = {978-3-319-27563-5},
    url = { http://www.springer.com/gp/book/9783319275635 },
    note = { http://www.cs.put.poznan.pl/kkrawiec/bps }
 }
~~~~~

Credits
=======

Much of the inspiration for this library comes from chatting with Jerry Swan, whom I'm deeply indebted for his advice and encouragement. 

