package com.janeirodigital.shapetrees;

import com.janeirodigital.shapetrees.enums.HttpHeaders;
import com.janeirodigital.shapetrees.helper.GraphHelper;
import com.janeirodigital.shapetrees.helper.HttpClientHelper;
import com.janeirodigital.shapetrees.helper.HttpHeaderHelper;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.apache.jena.graph.Graph;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

@Slf4j
public class RemoteResource {

    private static final String REL_TYPE = "type";
    private static final String LDP_CONTAINER = "http://www.w3.org/ns/ldp#Container";

    private final URI URI;
    private String authorizationHeaderValue;
    private Boolean invalidated = false;
    private Boolean exists;
    private Map<String, List<String>> responseHeaders;
    private Map<String, List<String>> parsedLinkHeaders;
    private Graph parsedGraph;
    private String rawBody;

    public RemoteResource(String uriString, String authorizationHeaderValue) throws IOException {
        URI requestUri;
        try {
            requestUri = new URI(uriString);
        } catch (URISyntaxException ex) {
            throw new IOException("Request URI is not a value URI");
        }
        this.URI = requestUri;
        this.authorizationHeaderValue = authorizationHeaderValue;
        dereferenceURI();
    }

    public RemoteResource(URI uri, String authorizationHeaderValue) throws IOException {
        this.URI = uri;
        this.authorizationHeaderValue = authorizationHeaderValue;
        dereferenceURI();
    }

    public RemoteResource(Response response) throws IOException, URISyntaxException {
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

    public String getBody() {
        if (!this.exists) return null;

        if (this.invalidated) {
            log.debug("RemoteResource#getBody({}) - Resource Invalidated - Refreshing", this.URI);
            dereferenceURI();
        }

        return this.rawBody;
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

        StringWriter sw = new StringWriter();
        RDFDataMgr.write(sw, updatedGraph, Lang.TURTLE);

        OkHttpClient httpClient = HttpClientHelper.getClient(true);
        Request request = new Request.Builder()
                .url(this.URI.toURL())
                .addHeader(HttpHeaders.CONTENT_TYPE.getValue(), "text/turtle")
                .post(RequestBody.create(sw.toString(), MediaType.get("text/turtle")))
                .build();

        httpClient.newCall(request).execute();

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

        OkHttpClient httpClient = HttpClientHelper.getClient(true);
        Request request = new Request.Builder()
                .url(this.URI.toURL())
                .addHeader(HttpHeaders.AUTHORIZATION.getValue(), this.authorizationHeaderValue)
                .build();

        Response response = httpClient.newCall(request).execute();

        parseResponseToRemoteResource(response);
        this.invalidated = false;
    }

    private void parseResponseToRemoteResource(Response response) throws IOException {
        // TODO I don't like this test
        this.exists = response.code() != 404;

        // Parse the headers for ease of use later
        if (response.headers() != null) {
            this.responseHeaders = response.headers().toMultimap();
        }

        // We especially care about Link headers which require extra parsing of the rel values
        if (this.responseHeaders.get(HttpHeaders.LINK.getValue()) != null) {
            this.parsedLinkHeaders = HttpHeaderHelper.parseLinkHeadersToMap(response.headers(HttpHeaders.LINK.getValue()));
        }

        // Save raw body
        this.rawBody = response.body().string();
    }

}
