ScEvo = Evolutionary Computation framework in Scala
===================================================
Krzysztof Krawiec
krawiec@cs.put.poznan.pl
June-Sept 2014

General philosophy
===================

We consider the space of Solutions (candidate solutions). 
A Solution can undergo an evaluation, the result of which is an Evaluation. 
A Solution accompanied with its Evaluation is an EvaluatedSolution. 
Evaluation can be anything that implements partial order, in particular ScalarEvaluation (e.g., fitness) or MultiobjectiveEvaluation. 

A space of Solutions can be searched using an IterativeAlgorithm (e.g., Evolution). 
At any given moment, an IterativeAlgorithm is in a certain State, which is a set of one or more EvaluatedSolutions. 
An IterativeAlgorithm can use one or more StoppingCondition to decide whether search should be terminated. 

To move from one State to another, IterativeAlgorithm uses a SearchStep. 
SearchStep takes the previous history of States (composed of at least the last State), and produces a new State. 
A SearchStep can (but does not have to) refer to solutions' Evaluation; in that case it is a SearchStepWithEval

A SearchStepWithEval selects a (nonempty) subset of EvaluatedSolutions; call them parents. 
Then it applies one or more search operator to the parents. 
A search operator is a function that accepts one or more EvaluatedSolution as arguments. 
Every application of a search operator results in one or more Solution (not evaluated anymore). 
The set of created Solutions ('children') undergoes evaluation, which results in a set of EvaluatedSolutions, i.e., new search State. 
Note that a Solution may not pass the evaluation stage (e.g., because it violates some constraint). 

To conduct a search in a given Solution space using an IterativeAlgorithm, you should create an instance of Experiment. 
The experiment will parse the command line parameters, set up the initial State, SearchStep, etc.
Then you call its launch() method. 
The Experiment conducts the search, collects its results in ResultDatabase, and saves its contents to an res*.txt file. 

For an example, see examples.MaxOnes

More details
============

* The evaluation process may reveal that the solution being evaluated is infeasible. This is why an evaluation function should be Solution => Option[EvaluatedSolution]. 
* Many search algorithms evaluate solutions in the context of other solutions in population (e.g., coevolutionary algorithms). For this reason, SearchStep assumes that evaluation is a function Seq[Solution] => Seq[EvaluatedSolution]
* To meet the requirements of various methods, selection takes place in two phases: 
- First, a Selector object is created. 
- Then, the Selector object is used like an iterator, which returns a solution with every call of next() method. 
* A search operator is allowed to fail, in which case it returns an empty list of Solutions. 

Technical
=========

* The randomized operations rely on tools.TRandom, which is intended to serve as a wrapper for java.util.Random, 
or any other (possibly better) random number generator. 
* Except for Evolution and Experiment, classes are implemented as immutable.
* Passing random number generators to many methods may be annoying, but allows avoiding global variables. 

TODO
====

* Consider having single global random number generator in Preamble (or in general a map Thread.id() => TRandom)
* Implement a straightforward island model
* Possibly: detach evaluation from solution; keep a mapping from solutions to evaluations

Done
====

* Fix NSGA

Older notes
===========

/* 
 * Assumptions:
 * - State captures the 'knowleable' about the search pocess; it has focus on EvaluatedSolutions; the non-evaluated solutions are not reflected in the state
 * - should support multi-arg search operators
 * - should support batch selection as well as pipes (one by one selection)
 * - SearchAlgorithm is applied to Seq[State], which should contain at least the previous state
 * - IterativeAlgorithm should not see other solutions than EvaluatedSolutions
 * - finding ideal should be also a stopping condition
 * - bestOfGen should be moved to postIterationCallback?


 */