package org.clj_tuples;

public class PatternMatchException extends Exception {

    public PatternMatchException() { }

    public String getMessage() {
        return new String("Error in pattern matching");
    }
}