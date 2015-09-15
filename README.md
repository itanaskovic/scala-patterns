<h1>Scala Patterns</h1>

<h2>Scope</h2>
This small projects serves as a minimal repository of know how how that I gather out of my own experience.

There is no clear objective to cover GoF design patterns or the functional design patterns. However, over time, some of my favorite GoFs might end up here.
<h2><a href="#patterns"/>Patterns</h2>
<h3><a href="#multifun"/>MultiFun</h3>
This ridiculous pattern came as a result of having to solve the problem of having one input, a list of functions and the need for multiple outputs.

One can think of it as a a list of inputs of the same type that need to be transformed into a different list of outputs of the same or a different type.

Normally, and probably in most of the cases one will build a class or a data structure that will hold the result of the complex computations.

In my case I decided that there I want more flexibility and I want to build different 'processors' with complex computations, producing different results without being bound to define a different structure for each result type.

The most simple solution is to simply create a sequence of functions and apply map(input) to get the results, however, one needs to be careful to add compatible functions (same domain and codomain). 

In essence the MultiFun is a function of <code>A => Seq[B]</code> which results from applying the same input A to a sequcen of functions <code>A => B</code>.

<h4>Recommendations</h4>
<ul>
<li>Use case classes for the input parameter instead of tuples. It adds a little overhead, but in the case of this pattern it makes sense.</li>
<li>If possible it is better to use a specific return type, e.g. Seq[String] or Seq[Specific_Class] rather than Seq[Any].</li>
<li>If the library of MultiFuns is growing it is better to use named functions (NamedFun)
</ul>

<h4>Usage Sample</h4>
<code>
...

    case class Product(name: String, specifications: Map[String, String])
    case class Price(value: Double, currency: String)

    def getPriceFromStore(product: Product, url: URL): Price = ???

    def getPriceFromAmazon_COM(product: Product) = getPriceFromStore(product, AMAZON_COM_URL)
    def getPriceFromAmazon_UK(product: Product) = getPriceFromStore(product, AMAZON_UK_URL)
    def getPriceFromAmazon_DE(product: Product) = getPriceFromStore(product, AMAZON_DE_URL)
    def getPriceFromEBay(product: Product) = getPriceFromStore(product, EBAY_URL)
    def getPriceFromAlibaba(product: Product) = getPriceFromStore(product, ALIBABA_URL)

    def getAmazonPrices = MultiFun(List(NamedFun("amazon.com", getPriceFromAmazon_COM),
      NamedFun("amazon.uk", getPriceFromAmazon_UK),
      NamedFun("amazon.de", getPriceFromAmazon_DE)))

    def getOffersMainStores = getAmazonPrices ++ MultiFun(List(NamedFun("ebay", getPriceFromEBay),
      NamedFun("alibaba", getPriceFromAlibaba)))

    val interestingProduct = Product("Programming in Scala", Map("author" -> "Martin Odersky and Lex Spoon", "type" -> "Paperback"))

    val amazonPrices = getAmazonPrices(interestingProduct)
    val mainStoresPrices = getOffersMainStores(interestingProduct)
. . . 
</code>

<h4>Known Problems, Limitations</h4> 
<ul>
<li>Because of type erasure it is difficult to infer the type of the input parameter, espacially when using tuples. Even though it adds a little overhead it is better to write a case class for the input instead of using a tuple.</li>

</ul>

<h2><a href="#references"/>References</h2>
<ul>
<li>Scala Design Patterns: Patterns for Practical Reuse and Design <i>by John Hunt</i></li>
<li>Design Patterns: Elements of Reusable Object-Oriented Software <i>by Erich Gamma, Richard Helm, Ralph Johnson, John Vlissides</i></li>
</ul>