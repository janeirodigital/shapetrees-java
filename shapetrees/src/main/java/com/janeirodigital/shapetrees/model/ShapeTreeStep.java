package com.janeirodigital.shapetrees.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

@Getter @Setter @NoArgsConstructor @AllArgsConstructor
public class ShapeTreeStep {
    private String id;
    private String rdfResourceType;
    private String shapeUri;
    private String label;
    private String uriTemplate;
    private List<URI> contents;
    private List<ReferencedShapeTreeStep> references;

    public URI getURI() throws URISyntaxException {
        return new URI(this.id);
    }

    public Boolean isContainer() {
        if (this.getRdfResourceType() != null && this.getRdfResourceType().equals("ldp:Container")) {
            return true;
        }
        return false;
    }
}
