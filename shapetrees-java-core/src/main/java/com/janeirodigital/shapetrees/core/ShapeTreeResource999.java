package com.janeirodigital.shapetrees.core;

import com.janeirodigital.shapetrees.core.enums.HttpHeaders;
import com.janeirodigital.shapetrees.core.enums.LinkRelations;
import com.janeirodigital.shapetrees.core.enums.ShapeTreeResourceType;
import com.janeirodigital.shapetrees.core.vocabularies.LdpVocabulary;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.jena.graph.Graph;

import java.net.URI;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@Getter @Slf4j
public class ShapeTreeResource999 {
    protected URI uri;
    protected boolean exists;
    @Setter protected String body;
    protected ResourceAttributes attributes;
    protected Optional<URI> associatedUri;
    protected String name;
    protected ShapeTreeResourceType resourceType;
    protected boolean container;
    protected boolean metadata;
    protected boolean managed;
    protected Optional<Graph> graph;

    @Override
    public String toString() {
        return "ShapeTreeResource999{" +
                "uri=" + this.uri +
                ", associatedUri=" + this.associatedUri +
                ", name='" + this.name + '\'' +
                ", body='" + this.body + '\'' +
                ", type=" + this.resourceType +
                ", exists=" + this.exists +
                ", container=" + this.container +
                ", metadata=" + this.metadata +
                ", managed=" + this.managed +
                ", attributes=" + this.attributes +
                '}';
    }

    protected ShapeTreeResource999() {
    }

}
