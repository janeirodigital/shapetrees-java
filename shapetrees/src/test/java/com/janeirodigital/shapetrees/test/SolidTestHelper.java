package com.janeirodigital.shapetrees.test;

import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.helper.HttpClientHelper;
import lombok.SneakyThrows;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import com.janeirodigital.shapetrees.enums.HttpHeaders;
import org.apache.http.util.EntityUtils;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;

import java.util.List;

public class SolidTestHelper {
    @SneakyThrows
    public static List<String> getRecursiveContainerContents(String authHeaderValue, String uri, List<String> entries) {
        CloseableHttpClient client = HttpClientHelper.getClient(true);
        HttpGet get = new HttpGet(uri);
        get.setHeader(HttpHeaders.ACCEPT.getValue(), "text/turtle");
        get.setHeader(HttpHeaders.AUTHORIZATION.getValue(), authHeaderValue);
        CloseableHttpResponse response = client.execute(get);

        HttpEntity entity = response.getEntity();
        String responseString = EntityUtils.toString(entity, "UTF-8");

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
        CloseableHttpClient client = HttpClientHelper.getClient(true);
        HttpDelete delete = new HttpDelete(uri);
        delete.setHeader(HttpHeaders.AUTHORIZATION.getValue(), authHeaderValue);
        CloseableHttpResponse response = client.execute(delete);
    }
}
