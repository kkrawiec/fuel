ScEvo = Evolutionary Computation framework in Scala
===================================================
Krzysztof Krawiec
krawiec@cs.put.poznan.pl
June-Dec 2014

General philosophy
===================

This is a one-page description of the basic components and the relations between them. The names of components are capitalized. Most of them are in the scevo.evo package. 

We consider a space of Solutions (candidate solutions). 
A Solution can undergo an evaluation, the result of which is an Evaluation. 
A Solution accompanied with its Evaluation is an EvaluatedSolution (the default implementation of EvaluatedSolution is ESol). 
Evaluation can be anything that implements partial order, in particular ScalarEvaluation (e.g., fitness) or MultiobjectiveEvaluation. 

A space of Solutions can be searched using an Algorithm. 
A particular form of Algorithm is IterativeAlgorithm. 
An IterativeAlgorithm executes a Prologue (which provides an InitialState), zero or more Steps, and Epilogue.  

At any given moment, the search process realized by an IterativeAlgorithm is in a certain State.
A State can be anything, but most frequently it is a PopulationState, which is a list of one or more EvaluatedSolutions (note: a *list*, so duplicates are permitted). 
An IterativeAlgorithm can use one or more StoppingCondition to decide whether search should be terminated. 

To move from one State to another, IterativeAlgorithm uses a Steep.
SearchStep takes the previous history of States (composed of at least the last State), and produces a new State. 
A SearchStep can (but does not have to) refer to solutions' Evaluation.

When the considered states are PopulationStates, we usually use SearchStepStochastic.
SearchStepStochastic selects a (nonempty) subset of EvaluatedSolutions; call them parents. 
Then it applies one or more SearchOperators to the parents. 
A search operator is a function that accepts one or more EvaluatedSolution as arguments. 
Every application of a search operator results in one or more Solution (not evaluated anymore). 
The set of created Solutions ('children') undergoes evaluation, which results in a set of EvaluatedSolutions, i.e., new search State. 
Note that a Solution may not pass the evaluation stage (e.g., because it violates some constraint). 

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
