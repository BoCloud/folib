package com.folib.nuget.indexer.symbols.exception;



public class PDBParseException extends RuntimeException {
    public PDBParseException(String message) {
        super(message);
    }

    public PDBParseException(Throwable cause) {
        super(cause);
    }
}