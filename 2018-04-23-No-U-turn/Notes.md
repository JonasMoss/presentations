
Oppgave 5: Ikke klart hva som er hypotesen skal testes. 

# Notes on No U-turn sampling

## Outline

### Five minutes recap
Spend five minutes on a HMC recap. Use the slides.

### Give short overview of paper
* The two objectives:
  - Choice of L
  - Choice of epsilon
* Huge impact:
  - ~ 700 cites
  - Basis of STAN, the modern BUGS.
  - STAN is popular and extremely flexible.
  - Give example syntax.
* Unclear to me what recent developments are.
  - An active research area, but somewhat hard to enter (by statistics standards).
  - Few references to Markov Chain theory.
  - Reads like engineering literature?
* Will only cover the No U-turn sampling in this talk.

### No U-turn sampling
* Explain the intuition:
  - Draw some trajectories and point at the normal trajectory.
  - Complain that it isn't very well motivated.
  - Most trajectories will be (in some sense) ergodic: 
    - They will fill the entire phase-space.
    - Hence they will be self-crossing and 'non-elliptical'.
    - I don't know how well this is studies or thought about.
    - Makes it impossible to lift the intuition about uniform sampling.
* Problem: Time-reversibility
  - Not possible to go back with the same probability.
  - Needed for time-reversibility: Q(x|x')P(x') = Q(x'|x)P(x) (P(x) = P(x'))
* How to fix it?
  - Idea: Make the No U-turn sampler time-reversible.
  - Construct a path by 'random doubling'

```python
while TRUE:
    direction = sample(left, right)
    simulation = simulation + simulate(direction)
    if(U-turn) break
    if(small probability) break
    
```
* The U-turn applies to each subtree.
* Why is this one time-reversible?
  - Need to filter out illegal elements.
  - Need: P(B, C | q') = P(B, C | q) when q is on C.
  - "The doubling procedure was stopped because either equation 3 or Equation 4 was
    satisfied by a state or subtree added during the final doubling iteration. In this case
    we must exclude from C any element of B that was added during this final doubling
    iteration, since starting the doubling process from one of these would lead to a stopping
    condition being satisfied before the full tree corresponding to B has been built."
* Now sample uniformly from C.


## Comments 

* On p. 4: "If L is too large, then HMC will generate trajectories that loop back and retrace their steps." 
  - Is this true in general? If the Hamiltonian is ergodic, it will fill the entire space, and might never revisit the same point. 
  - Easy to make both an example of and a counter-example to this.

## Understanding
* On p. 4: "Below, we present the No-U-Turn Sampler (NUTS), an extension of HMC that eliminates
the need to specify a fixed value of L. In Section 3.2 we present schemes for setting epsilon based
on the dual averaging algorithm of Nesterov (2009)"
  - Selecting L: No U-turn sampler
  - Selecting epsilon: Dual averaging
* On p. 5: Use a metric to stop when the distance get smaller. 
  - Not time-reversible (explain why)
  - "NUTS overcomes this issue by means of a     recursive algorithm that preserves
    reversibility by running the Hamiltonian simulation both forward and backward in time."