# shapetrees-java

[![Coverage](https://sonarcloud.io/api/project_badges/measure?project=xformativ_shapetrees-java&metric=coverage&token=648f4f007e540678a04a39c974cdf0046567acec)](https://sonarcloud.io/dashboard?id=xformativ_shapetrees-java)
[![Bugs](https://sonarcloud.io/api/project_badges/measure?project=xformativ_shapetrees-java&metric=bugs&token=648f4f007e540678a04a39c974cdf0046567acec)](https://sonarcloud.io/dashboard?id=xformativ_shapetrees-java)
[![Vulnerabilities](https://sonarcloud.io/api/project_badges/measure?project=xformativ_shapetrees-java&metric=vulnerabilities&token=648f4f007e540678a04a39c974cdf0046567acec)](https://sonarcloud.io/dashboard?id=xformativ_shapetrees-java)
[![Code Smells](https://sonarcloud.io/api/project_badges/measure?project=xformativ_shapetrees-java&metric=code_smells&token=648f4f007e540678a04a39c974cdf0046567acec)](https://sonarcloud.io/dashboard?id=xformativ_shapetrees-java)
[![Maintainability Rating](https://sonarcloud.io/api/project_badges/measure?project=xformativ_shapetrees-java&metric=sqale_rating&token=648f4f007e540678a04a39c974cdf0046567acec)](https://sonarcloud.io/dashboard?id=xformativ_shapetrees-java)
[![Reliability Rating](https://sonarcloud.io/api/project_badges/measure?project=xformativ_shapetrees-java&metric=reliability_rating&token=648f4f007e540678a04a39c974cdf0046567acec)](https://sonarcloud.io/dashboard?id=xformativ_shapetrees-java)
[![Security Rating](https://sonarcloud.io/api/project_badges/measure?project=xformativ_shapetrees-java&metric=security_rating&token=648f4f007e540678a04a39c974cdf0046567acec)](https://sonarcloud.io/dashboard?id=xformativ_shapetrees-java)

shapetrees-java is a Java (JDK11+) implementation of the [shape tree specification](https://shapetrees.org/TR/specification/index.html).

As detailed in the specification shape tree validation can be performed on either client or server sides.

Each module within the project has its own README.md to further explain its contents:
 * [shapetrees-java-core](shapetrees-java-core/README.md) - Core validation functionality and any classes required
   to interact with the API are present here
* [shapetrees-java-client-http](shapetrees-java-client-http/README.md) - Provides an (abstract) implementation of ShapeTreeClient for HTTP clients
* [shapetrees-java-client-core](shapetrees-java-client-core/README.md) - Defines a proposed interface for any shape
   tree client implementations
 * [shapetrees-java-okhttp](shapetrees-java-okhttp/README.md) - Provides a shape tree client implemented
   with [OkHttp](https://github.com/square/okhttp/).  This client can perform client-side
   validation by using an interceptor or interact directly with a server that performs server-side validation

