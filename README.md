ScEvo = Evolutionary Computation framework in Scala
===================================================
Krzysztof Krawiec
krawiec@cs.put.poznan.pl
June 2014 - Oct 2015

Introduction
===================

ScEvo is a succinct framework for implementing metaheuristic algorithms, in particular evolutionary algorithms, in Scala. It is largely written in the functional style, using object-oriented approach mainly on the top level. The library is organized as a collection of components, which in general are allowed to be stateful. 

A metaheuristic algorithm is a (usually compound) function. ScEvo's role is to help a user to build such a function using the components available in the library, facilitate running such algorithms, and provide convenient ways of parameterizing them and collecting results. 

This is a one-page description of the basic components and the relations between them. The names of components are capitalized. 

General philosophy
===================

Metaheuristics are iterative algorithms that iterate over States. A single step of such iteration is a function State => State. The object Iteration constructs an iterative algorithm (a function State => State) with a given step function (also a function State => State) and stopping/termination condition(s) (functions State => Boolean). 

State can be basically anything - we only require it to hold the iteration number. Most commonly, a State will hold a candidate solution or a population of candidate solutions. For the latter case, we provide StatePop trait and Population class; technically, population holds a *list* of solutions, so duplicates are permitted. If there is need of storing some form of history of search process (like e.g. in Tabu search), this should also be done in State.  

Solutions can be anything and in ScEvo they are used as parameters of generic types/classes, typically denoted by S. So Population[Vector[Int]] is a population of solutions being vectors of Ints. 

A solution can undergo an evaluation, the outcome of which can be anything (though in practice evaluations are usually Doubles, Ints, in general types implementing complete orders). Evaluation is another parameter of many generics in ScEvo, denoted by E. We often need to store solutions with their evaluations; the convention we use for that purpose is Scala's Tuple2 type. So for instance Population[Tuple2[Seq[Int],Double]] (or more succinctly Population[(Seq[Int],Double)]) is a population of solutions evaluated by Doubles. 

New solutions are built from the existing ones using search operators, i.e. in general functions of signature S x S x ... x S => S x ... x S. For instance a typical mutation operator is S => S. Such an operator is 'wrapped' using the SearchOperator trait. This trait is extended with convenience classes SearchOperator1, SearchOperator2, etc. for search operators of different arity.  

To use search operator in 

---
The recommended order looking at examples is: MaxOnes, MaxOnesVectors, Hiff, Rosenbrock, TSP, TSPMultiobjective. 

TODO: there should be a possibility to modify an option (?)


Most components have both 'regular' constructors (parameter passed explicitly via constructor argument) and constructors that fetch parameters from Options. 

A generic search algorithm is composed by the EA class. It is simply initialization followed by evaluation, and then followed by multiple iterations of breeding followed by evaluation. The class also decorates this workflow with per-iteration and final reporting. 

Quite often there is need for using multiple search operators (in parallel), 

An IterativeAlgorithm executes a Prologue (which provides an InitialState), zero or more Steps, and Epilogue.  

When the considered states are PopulationStates, we usually use SearchStepStochastic.
SearchStepStochastic selects a (nonempty) subset of EvaluatedSolutions; call them parents. 
Then it applies one or more SearchOperators to the parents. 
A search operator is a function that accepts one or more EvaluatedSolution as arguments. 
Every application of a search operator results in one or more Solution (not evaluated anymore). 
The set of created Solutions ('children') undergoes evaluation, which results in a set of EvaluatedSolutions, i.e., new search State. 
Note that a Solution may not pass the evaluation stage (e.g., because it violates some constraint). 

Evaluation can be anything that implements partial order, in particular ScalarEvaluation (e.g., fitness) or MultiobjectiveEvaluation. 

The trait scevo.evo.EA implements a 'vanilla' generational Evolutionary Algorithm. 
Apart from the above components, it requires:
* a Randomness (which provides a random number generator), 
* Options (which provides options to set components' parameters), and
* a Collector (which offers the functionality to store experiment results). 
Typically, we use OptionsFromArgs, which builds options by parsing the command line and also provides a default Collector which stores the results in a text file. 

To conduct a search using EA, your setup will need to involve also Experiment (see, e.g., scevo.examples.MaxOnes). 
The Experiment will run the Algorithm and safeguard against exceptions, reporting them via Collector. 
An experiment can be started using the launch() method. 
The Experiment runs the (instrumented) Algorithm, collects the results, and reports them usint the Collector. 

For an example, see scevo.examples.MaxOnes

More details
============

* Some search algorithms evaluate solutions in the context of other solutions in population (e.g., coevolutionary algorithms). For this reason, SearchStep assumes that evaluation is a function Seq[Solution] => Seq[EvaluatedSolution]
* To meet the requirements of various methods, selection takes place in two phases: 
- First, a Selector object is created. 
- Then, the Selector object is used like an iterator, which returns a solution with every call of next() method. 
* A search operator is allowed to fail, in which case it returns an empty list of Solutions. 

Non-essential components
============

* Multiobjective selection: scevo.evo.NSGA
* Interaction functions between candidate solutions and tests: scevo.evo.Iteration
* Preliminary implementation of two-population coevolution: scevo.evo.Coevolution

Technical
=========

* Except for Evolution and Experiment, classes are implemented as immutable.
* The randomized operations rely on tools.TRandom, which is intended to serve as a wrapper for java.util.Random, or any other (possibly better) random number generator. 
