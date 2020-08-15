package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.helper.HttpClientHelper;
import lombok.SneakyThrows;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.util.List;

public class SolidTestHelper {
    @SneakyThrows
    public static List<String> getRecursiveContainerContents(String authHeaderValue, String uri, List<String> entries) {
        OkHttpClient client = HttpClientHelper.getClient();
        Request get = new Request.Builder()
                .url(uri)
                .addHeader(HttpHeaders.ACCEPT.getValue(), "text/turtle")
                .addHeader(HttpHeaders.AUTHORIZATION.getValue(), authHeaderValue)
                .build();

        Response response = client.newCall(get).execute();

        String responseString = response.body().string();

        Graph graph = GraphHelper.readStringIntoGraph(responseString, "text/turtle");
        List<Triple> containmentTriples = graph.find(null, NodeFactory.createURI("http://www.w3.org/ns/ldp#contains"), null).toList();

        for (Triple containmentTriple : containmentTriples) {
            entries.add(containmentTriple.getObject().getURI());
            getRecursiveContainerContents(authHeaderValue, containmentTriple.getObject().getURI(), entries);
        }

        return entries;
    }

    @SneakyThrows
    public static void deleteResource(String authHeaderValue, String uri) {
        OkHttpClient client = HttpClientHelper.getClient();

        Request delete = new Request.Builder()
                .url(uri)
                .addHeader(HttpHeaders.AUTHORIZATION.getValue(), authHeaderValue)
                .delete()
                .build();

        client.newCall(delete).execute();
    }
}
