My defn.
--------
E kills D if D - E is unwinnable.
Ie. |

Chapter 1 The Picard and Jacobian Groups
========================================

Definition of |.|
-----------------
|D| = {E ∈ Div(G) : E ~ D and E ≥ 0}
Called "complete linear system of D".
Ie. all the winning positions you can get to.

Chapter 2 The Discrete Laplacian
--------------------------------
|D| = D + Prin(G)
|0| = Prin(G)

Chapter 3
=========
G is d-edge connected if it stays connected after the removal of d-1 edges.

3.1 greedy
----------
In debt people keep borrowing but not everyone allowed to borrow.
Greedy algorithm always succeeds if possible to win.
Loop only happens if impossible to win.

Proposition 3.1
---------------
Greedy algorithm always produces same result whatever vertex order
is chosen.

In English: plans σ₁ and σ₂.
Let σ̃₂ be σ₂ where we stop just before some vertex w borrows more than in σ₁.
Then σ̃₂ borrows same as σ₁ at w, but borrows less everywhere else.
So w in plan σ̃₂ has at least as much money as in σ₁.
So w has no debt. So why would it borrow at next step?

Proposition 3.6
---------------
Let D ∈ Div(G) and let ≺ be tree ordering rooted at q ∈ V
Let D' be divisor obtained from D by firing non-empty S ⊂ Ṽ.
Then D' ≺ D.

Proof
-----
Obvious. Closest point in S to q must squirt cash out towards q.

Proposition 3.7
---------------
Choose q. Let D ∈ Div(G).
∃! q=reduced divisor equivalent to D.

3.2 q-reduced
-------------
Legal set-firing S: each vertex loses amount = # nbrs outside S = outdeg_S(v).
This is >=0 so after set firing, money goes down.
So money just flows out of set.

q-reduced means no S ∌ q can be set-fired.

q-reduced
---------
D(v) ≥ 0 for all V ∈ Ṽ := V\{q}
Choose tree ordering based at q.
D' ≺ D if D' has more cash closer to q.

Proposition 3.10
----------------
Suppose G is d-edge connected. Every effective divisor on G of degree < d is q-reduced.

Proof
-----
Suppose E has degree k < d and let S ⊂ Ṽ a non-empty set of non-sources.
Edges connecting S to V\S ≠ ∅ is given by ∑(v∈S) outdeg_S(v) because
G is d-connected.
So
    ∑(v∈S) E(v) ≤ k < d ≤ ∑(v∈S) outdeg_S(v)

There must be some v ∈ S s.t. E(v) < outdeg_S(v).
So S can't actually be fired.

Roughly: if graph is well-connected, some points are expensive to fire.
So all points in S in small divisor aren't enough to cover outgoings from S.

Proposition 3.11
----------------
G d-edge connected
D ∈ Div(G), winnable, deg(D) < G.
|D| is single effective divisor.
For each k ≥ d, ∃ linear system of degree k with more than one effective divisor.
Ie. more than one way to win.

Proof
-----
D is winnable.
So reacahble winnable position.
Say E₁ ~ E₂ ~ D. (E₁, E₂ effective.)
deg(E₁) = deg(E₂) = deg(D) < d
E₁ and E₂ are both q-reduced. By Theorem 3.2.1 they're equal

XXX <- Come back

Chapter 5 Riemann-Roch
======================

Effective divisor: all ≥ 0
Principal divisor: "firing script"
(Does this mean ~ 0?)

Ex 5.1
------
RTP r(D) ≤ max(-1,deg(D))

Definition of r(.)
------------------
For k ≥ 0:
r(D) ≥ k ⇔ (∀E deg(E) = k ⇒ |D-E| ≠ ∅)
r is how safe you are. r(D) ≥ k means no divisor of degree k makes
D unwinnable.

So r(D) ≥ k means no k-divisor kills D
   r(D) < k means there's a k-divisor that kills D
To find upper bound on r(D) find a single killer.

Suppose r(D) ≥ k = deg(D) + 1.

r(D) ≥ k ⇔ (∀E deg(E) = deg(D) + 1 ⇒ |D-E| ≠ ∅)
But deg(D-E) = -1
So |D-E| = ∅ ⨳
so r(D) < deg(D) + 1
deg(D)+1 ≥ 0 ⇒ r(D) ≤ deg(D)
deg(D) ≥ -1 ⇒ r(D) ≤ deg(D)
If deg(D) is any lower, r(D) is still -1 by definition.

In a nutshell, you can kill a divisor by subtracting a divisor of bigger
degree.

Ex 5.2
------
RTP deg(D) = ∅ ⇒ (r(D) = ∅ ⇔ D principal)

deg(D) = ∅ and r(D) = ∅ ⇒ D principal
deg(D) = ∅ and D principal ⇒ r(D) = ∅

⇐
If D is principal then clearly winnable. So r(D) ≥ 0
D - v unwinnable so r(D) < 1.

Conversely, suppose
deg(D) = ∅ and r(D) = ∅
r(D) ≥ 0 means D is winnable so principal

Ex 5.3
------
RTP
r(D) ≤ r(D + v) ≤ r(D) + 1 i.e. adding a dollar increases rank by at most 1

A winning play for D is a winning play for D+v so D+v is no harder than D
and so r(D) ≤ r(D + v)

Suppose E (of degree r(D)) kills D. Then E + v kills D + v
So r(D + v) <= deg (E + v) = r(D) + 1.

Ex 5.4
------
RTP r(D + D') ≥ r(D) + r(D') (r(D), r(D') non-negative)

If we can win D and D' we can use sum strategy to win D + D'.

Suppose E of degree r(D) + r(D')  kills D + D'.
Split E as E' + E'' of degrees r(D) and r(D').
E' can't kill D.
E'' can't kill D'.
So actually we have a strategy to win both D - E' and D' - E''
and therefore a strategy to win D + D' - E. So D + D' is immune to
attacks of degree r(D) + r(D').

Proposition 5.5
---------------
E ∈ Div₊(G), deg(E) < d. G d-edge connected.
Then r(E) = min{E(v): v ∈ V(G)}

Proof.

