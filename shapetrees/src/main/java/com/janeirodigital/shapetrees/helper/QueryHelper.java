package com.janeirodigital.shapetrees.helper;

public class QueryHelper {

    /**
     * Return a query that will remove all CSS managed triples that can't be included in a PUT request.
     * @return
     */
    public static String removeManagedTriplesQuery() {
        return "PREFIX ldp: <http://www.w3.org/ns/ldp#> \n" +
               "DELETE WHERE { ?s ldp:contains ?o . }";
    }

}
