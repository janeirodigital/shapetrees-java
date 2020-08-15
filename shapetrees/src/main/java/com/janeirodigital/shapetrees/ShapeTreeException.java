package com.janeirodigital.shapetrees;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.io.IOException;

@Getter @AllArgsConstructor
public class ShapeTreeException extends IOException {
    private final int statusCode;
    private final String message;
}
