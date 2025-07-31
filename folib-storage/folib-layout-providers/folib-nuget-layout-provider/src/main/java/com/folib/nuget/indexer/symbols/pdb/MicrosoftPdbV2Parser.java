package com.folib.nuget.indexer.symbols.pdb;


import com.folib.nuget.indexer.symbols.exception.UnsupportedPdbFileException;
import jakarta.annotation.Nullable;

public class MicrosoftPdbV2Parser extends PdbParser{
    public MicrosoftPdbV2Parser(byte[] fileBytes) {
        super(fileBytes);
    }

    @Nullable
    public PdbGuid parsePdbAndExtractGuid() {
        throw new UnsupportedPdbFileException("Microsoft C/C++ program database 2.00 is currently not supported");
    }
}
