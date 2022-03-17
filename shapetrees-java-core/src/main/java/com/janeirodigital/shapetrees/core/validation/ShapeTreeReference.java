package com.janeirodigital.shapetrees.core.validation;

import com.google.re2j.Matcher;
import com.google.re2j.Pattern;
import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.Getter;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.ResourceFactory;

import java.net.URL;
import java.util.Iterator;
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

    public static Property getPropertyFromReference(ShapeTreeReference reference) {
        Property property = null;
        if (reference.viaPredicate()) { property = ResourceFactory.createProperty(reference.getPredicate().toString()); }
        if (reference.viaShapePath()) {
            // TODO - this is a bit of a workaround to extract the target property from the shape path, given the
            // TODO - current lack of a shape path parser in java. It is functionally equivalent to viaPredicate
            Pattern pattern = Pattern.compile("@\\S+~(\\S*$)");
            Matcher matcher = pattern.matcher(reference.getShapePath());
            if (!matcher.matches()) return null;
            String parsed = matcher.group(1);
            if (parsed == null) return null;
            property = ResourceFactory.createProperty(parsed);
        }
        return property;
    }

    public static ShapeTreeReference findChildReference(ShapeTree shapeTree, URL childUrl) throws ShapeTreeException {
        try {
            Iterator<ShapeTreeReference> iterator = shapeTree.getReferencedShapeTrees();
            while (iterator.hasNext()) {
                ShapeTreeReference reference = iterator.next();
                if (reference.getReferenceUrl().equals(childUrl)) { return reference; }
            }
        } catch (ShapeTreeException ex) {
            throw new ShapeTreeException(500, "Failed to lookup shape tree references for shape tree: " + shapeTree.getId());
        }
        return null;
    }

}
