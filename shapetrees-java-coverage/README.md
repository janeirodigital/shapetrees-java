# shapetrees-java-coverage

This module generates an accurate JaCoCo code coverage report across all of the modules
of shapetrees-java. JaCoCo is designed to provide coverage reports on individual modules, each of which contains 
tests and the code being tested. In this project, we have one module that includes the test for the others.

The most reliable way to ensure we are generating full test coverage output is to have a seperate module (this one)
that generates an aggregate report. The [shapetrees-java-coverage/pom.xml](./pom.xml) does exactly this, including
the other modules as dependencies, which automatically include them in the aggregate output, which is generated
by the maven `test` goals (e.g. run `mvn clean test` on the command line), and stored in 
`target/site/jacoco-aggregate`.