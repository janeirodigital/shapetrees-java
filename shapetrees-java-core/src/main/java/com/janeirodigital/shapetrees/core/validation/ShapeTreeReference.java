package com.janeirodigital.shapetrees.core.validation;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.Getter;

import java.net.URL;
import java.util.Objects;

@Getter
public class ShapeTreeReference {

    final URL referenceUrl;
    final String shapePath;
    final URL predicate;

    public ShapeTreeReference(URL referenceUrl, String shapePath, URL predicate) throws ShapeTreeException {
        this.referenceUrl = Objects.requireNonNull(referenceUrl);
        if (shapePath == null && predicate == null) {
            throw new ShapeTreeException(500, "Shape tree reference must have either a shape path or a predicate");
        } else if (shapePath != null && predicate != null) {
            throw new ShapeTreeException(500, "Shape tree reference cannot have a shape path and a predicate");
        } else if (shapePath != null) {
            this.shapePath = shapePath;
            this.predicate = null;
        } else {
            this.predicate = predicate;
            this.shapePath = null;
        }
    }

    public boolean viaShapePath() {
        return shapePath != null;
    }

    public boolean viaPredicate() {
        return predicate != null;
    }

}
