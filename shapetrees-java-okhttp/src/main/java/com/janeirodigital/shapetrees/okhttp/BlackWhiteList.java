package com.janeirodigital.shapetrees.okhttp;

import com.janeirodigital.shapetrees.core.exceptions.ShapeTreeException;
import lombok.AllArgsConstructor;

import java.net.URL;
import java.util.Set;

@AllArgsConstructor
public class BlackWhiteList {

    private final Set<String> whiteListDomains;
    private final Set<String> blackListDomains;

    public void check(URL resourceURL) throws ShapeTreeException {
        if (this.blackListDomains != null && this.blackListDomains.contains(resourceURL.getHost())) {
            throw new ShapeTreeException(426, "Provided URL is on the configured black-list");
        }

        if (this.whiteListDomains != null && !this.whiteListDomains.contains(resourceURL.getHost())) {
            throw new ShapeTreeException(426, "Provided URL is NOT on the configured white-list");
        }
    }

}
