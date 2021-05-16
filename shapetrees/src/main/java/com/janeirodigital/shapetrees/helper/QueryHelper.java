package com.janeirodigital.shapetrees.helper;

public class QueryHelper {

    /**
     * Builds a query that upon applying to a graph will remove all triples that have a subject equal the the resource uri
     * passed in. This is meant to remove all server managed triples from the document so that the eventual PUT of the
     * modified resource succeeds in CSS. However, there is a real risk of removing unintended triples.
     * @param resourceURI
     * @return
     */
    public static String removeManagedTriplesQuery(String resourceURI) {
        return "DELETE { ?s ?p ?o } \n" +
               "WHERE { <" + resourceURI + "> ?p ?o }";
    }

}
