JAR_REPO=/home/eric/.m2/repository/

java -Dfile.encoding=UTF-8 \
     -classpath ../../../ericprud/java-to-typescript/javatots/target/classes:$JAR_REPO/org/yaml/snakeyaml/1.29/snakeyaml-1.29.jar:$JAR_REPO/com/github/javaparser/javaparser-core/3.23.1/javaparser-core-3.23.1.jar \
     org.javatots.main.JavaToTypescript javatots-config.yaml
