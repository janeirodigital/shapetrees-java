package com.janeirodigital.shapetrees.core.exceptions;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.io.IOException;

@Getter @AllArgsConstructor
public class ShapeTreeException extends Exception {
    private final int statusCode;
    private final String message;
}
