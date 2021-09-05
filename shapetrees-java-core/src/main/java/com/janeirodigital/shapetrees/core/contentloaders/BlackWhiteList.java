package com.janeirodigital.shapetrees.core.contentloaders;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.AllArgsConstructor;

import java.net.URI;
import java.util.Set;

@AllArgsConstructor
public class BlackWhiteList {
    private final Set<String> whiteListDomains;
    private final Set<String> blackListDomains;

    public void check(URI resourceURI) throws ShapeTreeException {
        if (blackListDomains != null && blackListDomains.contains(resourceURI.getHost())) {
            throw new ShapeTreeException(426, "Provided URI is on the configured black-list");
        }

        if (whiteListDomains != null && !whiteListDomains.contains(resourceURI.getHost())) {
            throw new ShapeTreeException(426, "Provided URI is NOT on the configured white-list");
        }
    }
}