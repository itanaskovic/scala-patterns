<h1>Scala Patterns</h1>

<h2>Scope</h2>
This small projects serves as a minimal repository of know how how that I gather out of my own experience.

There is no clear objective to cover GoF design patterns or the functional design patterns. However, over time, some of my favorite Gofs might end up here.

<h2>Contents</h2>

<h3>MultiFun</h3>
This ridiculous pattern came as a result of having to solve the problem of having one input, a list of functions and the need for multiple outputs.
One can think of it as a a list of inputs of the same type that need to be transformed into a different list of outputs of the same or a different type.

Normally, and probably in most of the cases one will build a class or a data structure that will hold the result of the complex computations.

In my case I decided that there I want more flexibility and I want to build different processors with complex computations, producing different results without being bound to define a different structure for each result type.

TODO: Figure out variance for the input and output types.

<h2>References</h2>
<ul>
<li>Scala Design Patterns: Patterns for Practical Reuse and Design <i>by John Hunt</i></li>
<li>Design Patterns: Elements of Reusable Object-Oriented Software <i>by Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides</i></li>
</ul>