package com.janeirodigital.solid.interoperability;

import com.janeirodigital.shapetrees.ShapeTreeEcosystem;

import java.net.URI;

public class SolidInteroperability {
    private final ShapeTreeEcosystem ecosystem;

    public SolidInteroperability() {
        this.ecosystem = getDefaultEcosystem();
    }

    public SolidInteroperability(ShapeTreeEcosystem ecosystem) {
        this.ecosystem = ecosystem;
    }

    public URI getDataRoot(URI webId) {
        // Dereference webID
        return null;
    }

    private ShapeTreeEcosystem getDefaultEcosystem() {
        return new SolidInteroperabilityEcosystem();
    }

}
