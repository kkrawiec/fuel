

TODO
====

* In SearchStep: Note: This loop will iterate forever is none of the search operators manages to produce a solution. 
* Make EvaluatedSolution Ordered; get rid of betterThan?
* Steady state EA
* Optimization: Pick the right collections
* introduce scaps.evo.Defaults
* Implement a straightforward island model
* Possibly: detach evaluation from solution; keep a mapping from solutions to evaluations

Older notes
===========

/* 

Done:
* Instruction set as parameter
* Reporter trait
* Report BestSoFar in snapshots


 * Assumptions:
 * - State captures the 'knowleable' about the search process; it has focus on EvaluatedSolutions; the non-evaluated solutions are not reflected in the state
 * - should support multi-arg search operators
 * - should support batch selection as well as pipes (one by one selection)
 * - SearchAlgorithm is applied to Seq[State], which should contain at least the previous state
 * - IterativeAlgorithm should not see other solutions than EvaluatedSolutions
 * - finding ideal should be also a stopping condition
 * - bestOfGen should be moved to postIterationCallback?

Not true anymore:
* The evaluation process may reveal that the solution being evaluated is infeasible. This is why an evaluation function should be Solution => Option[EvaluatedSolution]. 
* Passing random number generators to many methods may be annoying, but allows avoiding global variables. 

 */