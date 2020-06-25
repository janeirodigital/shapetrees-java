package com.janeirodigital.shapetrees;

import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.helper.HttpHeaderHelper;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.jena.graph.Graph;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

@Slf4j
public class RemoteResource {

    private static final String REL_TYPE = "type";
    private static final String LDP_CONTAINER = "http://www.w3.org/ns/ldp#Container";

    private final URI URI;
    private Boolean invalidated = false;
    private Boolean exists;
    private Map<String, List<String>> responseHeaders;
    private Map<String, List<String>> parsedLinkHeaders;
    private Graph parsedGraph;
    private String rawBody;

    public RemoteResource(String uriString) throws IOException {
        URI requestUri;
        try {
            requestUri = new URI(uriString);
        } catch (URISyntaxException ex) {
            throw new HttpResponseException(400, "Request URI is not a value URI");
        }
        this.URI = requestUri;
        dereferenceURI();
    }

    public RemoteResource(URI uri) throws IOException {
        this.URI = uri;
        dereferenceURI();
    }

    public RemoteResource(CloseableHttpResponse response) throws IOException, URISyntaxException {
        this.parseResponseToRemoteResource(response);
        this.URI = new URI(getFirstHeaderByName("Location"));
    }

    public URI getURI() {
        if (this.invalidated) {
            dereferenceURI();
        }
        return this.URI;
    }

    public Boolean exists() {
        return this.exists;
    }

    // Lazy-load graph when requested
    public Graph getGraph() {
        if (!this.exists) return null;

        if (this.invalidated) {
            log.debug("RemoteResource#getGraph({}) - Resource Invalidated - Refreshing", this.URI);
            dereferenceURI();
        }

        if (this.parsedGraph == null) {
            this.parsedGraph = GraphHelper.readStringIntoGraph(this.rawBody, getFirstHeaderByName(HttpHeaders.CONTENT_TYPE.getValue()));
        } else {
            return this.parsedGraph;
        }
        return null;
    }

    public Boolean isContainer() {
        return parsedLinkHeaders.get(REL_TYPE).contains(LDP_CONTAINER);
    }

    public String getFirstHeaderByName(String headerName) {
        if (this.invalidated) {
            log.debug("RemoteResource#getFirstHeaderByName({}) - Resource Invalidated - Refreshing", this.URI);
            dereferenceURI();
        }

        List<String> headerValues = responseHeaders.get(headerName);
        if (headerValues == null) {
            return null;
        }

        return headerValues.get(0);
    }

    public List<String> getHeaderValues(String headerName) {
        if (this.invalidated) {
            log.debug("RemoteResource#getHeaderValues({}) - Resource Invalidated - Refreshing", this.URI);
            dereferenceURI();
        }

        return responseHeaders.get(headerName);
    }

    public List<String> getLinkHeaderValuesByRel(String relation) {
        if (this.invalidated) {
            log.debug("RemoteResource#getLinkHeaderValuesByRel({}) - Resource Invalidated - Refreshing", this.URI);
            dereferenceURI();
        }

        return parsedLinkHeaders.get(relation);
    }

    public String getFirstLinkHeaderValueByRel(String relation) {
        if (this.invalidated) {
            log.debug("RemoteResource#getFirstLinkHeaderValueByRel({}) - Resource Invalidated - Refreshing", this.URI);
            dereferenceURI();
        }

        List<String> headerValues = parsedLinkHeaders.get(relation);
        if (headerValues == null) {
            return null;
        }

        return headerValues.get(0);
    }

    public void updateGraph(Graph updatedGraph, Boolean refreshResourceAfterUpdate) throws Exception {
        log.debug("RemoteResource#updateGraph({})", this.URI);

        if (this.invalidated) {
            throw new Exception("Cannot call 'updateGraph' on an invalidated RemoteResource - ");
        }

        CloseableHttpClient httpClient = HttpClients.createDefault();
        HttpPut putRequest = new HttpPut(this.URI);
        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, updatedGraph, Lang.TURTLE);
        putRequest.setEntity(new StringEntity(sw.toString(), StandardCharsets.UTF_8));
        putRequest.addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle");
        httpClient.execute(putRequest);

        if (refreshResourceAfterUpdate) {
            dereferenceURI();
        } else {
            this.invalidated = true;
            log.debug("RemoteResource#updateGraph({}) - Invalidating Resource", this.URI);
        }
    }

    @SneakyThrows
    private void dereferenceURI() {
        log.debug("RemoteResource#dereferencingURI({})", this.URI);
        CloseableHttpClient httpClient = HttpClients.createDefault();
        CloseableHttpResponse response = httpClient.execute(new HttpGet(this.URI));
        parseResponseToRemoteResource(response);
        this.invalidated = false;
    }

    private void parseResponseToRemoteResource(CloseableHttpResponse response) throws IOException {
        // TODO I don't like this test
        this.exists = response.getStatusLine().getStatusCode() != 404;

        // Parse the headers for ease of use later
        if (response.getAllHeaders() != null) {
            this.responseHeaders = HttpHeaderHelper.parseHeadersToMap(response.getAllHeaders());
        }

        // We especially care about Link headers which require extra parsing of the rel values
        if (this.responseHeaders.get(HttpHeaders.LINK.getValue()) != null) {
            this.parsedLinkHeaders = HttpHeaderHelper.parseLinkHeadersToMap(response.getHeaders(HttpHeaders.LINK.getValue()));
        }

        // Save raw body
        if (response.getEntity() instanceof StringEntity) {
            // TODO: Potential area for improvement to handle larger responses without blocking all to hell
            this.rawBody = IOUtils.toString(((StringEntity) response.getEntity()).getContent(), StandardCharsets.UTF_8);
        }
    }
}
