package com.janeirodigital.shapetrees.helper;

public class QueryHelper {

    /**
     * Return a query that will remove all CSS managed triples that can't be included in a PUT request.
     * @return
     */
    public static String removeManagedTriplesQuery() {
        return "DELETE { ?s ?p ?o } \n" +
               "WHERE { ?s <http://www.w3.org/ns/ldp#contains> ?o }";
    }

}
