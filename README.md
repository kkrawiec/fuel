ScEvo = Evolutionary Computation framework in Scala
===================================================
Krzysztof (Chris) Krawiec, krawiec@cs.put.poznan.pl
June 2014 - Oct 2015

Introduction
===================

ScEvo is a succinct framework for implementing metaheuristic algorithms, in particular evolutionary algorithms, in Scala. It is written primarily in functional style, with most objects implemented as immutable, and using object-oriented flavor mainly on the top level. The library is organized as a collection of components, which in general are allowed to be stateful. 

A metaheuristic algorithm is a compound function. ScEvo's role is to

* help a user to build such a function using the components available in the library, 
* facilitate running such algorithms/experiments, and 
* provide convenient ways of parameterizing the algorithms and collecting results. 

ScEvo provides relatively few top-level components (complete algorithms), because composing them from the pieces described below is really easy in functional programming paradigm. For this reason, it can be particularly useful for using metaheuristics in innovative ways, e.g., for hybridizing them with other algorithms, devising 'homebrew' algorithms, or playing with hyperheuristics. 

What follows is a one-page description of the basic components and the relations between them. The names of components (traits, classes, objects) are capitalized. 


General framework
===================

A metaheuristics is an iterative algorithm that iterates over States. A single step of such iteration is a function State => State. The object Iteration constructs an iterative algorithm (a function State => State) with a given step function (also a function State => State) and stopping/termination condition(s) (functions State => Boolean). 

State can be basically anything - we only require it to maintain the iteration number. Typically, a State will hold a candidate solution or a population of candidate solutions. For the latter case, we provide StatePop trait and Population class; population holds a *list* of solutions, so duplicates are permitted. If there is need of storing some form of history of search process (like e.g. in Tabu search), this should also be done in State.  

Solutions can be anything and they are parameters of generic types/classes, typically denoted by S. So Population[Vector[Int]] is a population of solutions being vectors of Ints. 

A solution can undergo an evaluation, the outcome of which can be anything (though in practice evaluations are usually Doubles, Ints, in general types implementing complete orders). Evaluation is another parameter of many generics in ScEvo, denoted by E. We often need to store solutions with their evaluations; the convention we use for that purpose is Scala's Tuple2 type. So for instance Population[Tuple2[Seq[Int],Double]] (or more succinctly Population[(Seq[Int],Double)]) is a population of solutions evaluated by Doubles. Multiobjective evaluation can be realized by assuming that evaluation is, e.g., a Seq[Double], and using an instance of Dominance trait (a kind of partial order). 

New solutions are built from the existing ones using search operators, i.e. in general functions of signature S x S x ... x S => S x ... x S. For instance a typical mutation operator is S => S. Such an operator is 'wrapped' using the SearchOperator trait. This trait is extended with convenience classes SearchOperator1, SearchOperator2, etc. for search operators of different arity.  

Quite often there is need for using multiple search operators. Use an instance of Moves to keep them together and present them to a search algorithm. A class extending Moves has to implement also a method for creating new solutions. The package scevo.moves provides basic implementations of moves for most popular solution representations: vectors of bits, vector of real numbers and for permutations. See moves.VectorMoves for an example.  

To use Moves in an iterative search, you should use Breeder. A breeder uses a selection operator (Selection) to select parent solutions from the current population, applies a search operator from Moves, obtaining so the children solutions, and then checks the children solutions for feasibility, rejecting the infeasible ones. A breeder repeats these steps until sufficiently many children are bred - in the case of the default SimpleBreeder, when the population of children is as big as the population of parents. SimpleBreeder applies the particular search operators from Moves according to the given probability distribution. 

The above components can be built either by calling constructors (or static methods in companion objects), or more automatically, based on the parameters provided by an Options object. 

The abstract class EACore provides the means for constructing basic evolutionary algorithms. Class SimpleEA extends that with simple reporting and default selection and breeding mechanisms (TournamentSelection and SimpleBreeder).  It is simply initialization followed by evaluation, and then followed by multiple iterations of breeding followed by evaluation. 


Setup and reporting
-------------------

Apart from the above components, it :
* Options (which provides options to set components' parameters), and
* a Collector (which offers the functionality to store experiment results). 
* a random number generator (an instance of TRandom). 

Use OptCollRng to obtain a triple of these objects by parsing a parameter string, or an Array of strings, for instance passed from the command line (see example.MaxOnesVector). An even more succinct way is to create an Env object that does it for you (see example.MaxOnes). 

The default Collector stores the results in a text file in the format of Java properties. By default, the file is named randomly and is guaranteed to be unique in the directory. To change its name and location use --outFile and --outDir options. 


Running a search 
----------------

The algorithm obtained by calling SimpleAE.algorithm is a regular function that can be launched individual. However, it may be worthy running it within an Experiment.  The Experiment will run the Algorithm and safeguard against exceptions, reporting them (and a few other things like timing) via a Collector. Use --printResults to print the results collected by Collector to standard output (and not only to file). 

Examples
--------

The package scevo.example presents several usage scenarios for discrete, continuous, and combinatorial optimization. 
The recommended order looking at them is: MaxOnes, MaxOnesVectors, Hiff, Rosenbrock, TSP, TSPMultiobjective. 

Package organization
--------------------

scevo.core: Elementary concepts: State, Dominance, BestSelector
scevo.func: Main components: Evaluation, Selection, Breeding, Algorithm, etc. 
scevo.example: Ready-to-run examples (runnable as independent programs or from Scala REPL; see illustration in MaxOnes). 
scevo.moves: Definitions of basic search operators for basic domains. 
scevo.util: Helper objects and functions


Technical comments
--------------------

For convenience, the components (particularly the top-level ones) often use implicit arguments (in particular for Options, Collector and RNG). This immensely reduces the number of parameters that need to be passed when constructing the components. Note that, to be used in that mode, a value has to be declared with the 'implicit' keyword. Obviously, if needed, those arguments can be alternatively passed explicitly. 

The randomized operations rely on tools.TRandom, which is intended to serve as a wrapper for java.util.Random, or any other (possibly better) random number generator. 
 
Some search algorithms evaluate solutions in the context of other solutions in population (e.g., coevolutionary algorithms). For this reason, SearchStep assumes that evaluation is a function Seq[Solution] => Seq[EvaluatedSolution]

A search operator is allowed to fail, in which case it returns an empty list of Solutions. 

Other components
----------------

* Multiobjective selection: scevo.evo.NSGA
* Interaction functions between candidate solutions and tests: scevo.evo.Iteration
* Preliminary implementation of two-population coevolution: scevo.evo.Coevolution


Credits
-------

Much of inspiration for this library comes from chats with Jerry Swan. 


Some justifications 
-------------------


