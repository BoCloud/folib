package com.folib.nuget.indexer.symbols.pdb;

import com.folib.nuget.indexer.symbols.exception.PDBParseException;
import com.folib.nuget.indexer.symbols.exception.UnsupportedPdbFileException;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public abstract class PdbParserFactory {
    public static final String EMPTY_FILE_ERROR_MESSAGE = "Cannot parse Pdb file, file's content is empty";
    public static final String UNSUPPORTED_FILE_ERROR_MESSAGE = "PDB format is not supported";
    public static final String MS_V2_UNSUPPORTED_ERROR_MESSAGE = "Microsoft C/C++ program database 2.00 is currently not supported";

    @NonNull
    public static PdbParser create(byte[] pdbFileBytes) {
        if (pdbFileBytes != null && pdbFileBytes.length > 0) {
            if (PdbUtils.isMicrosoftPdbV7(pdbFileBytes)) {
                log.debug("Pdb file is from type: Microsoft C/C++ MSF 7.00");
                return new MicrosoftPdbV7Parser(pdbFileBytes);
            } else if (PdbUtils.isMicrosoftPdbV2(pdbFileBytes)) {
                log.debug("Pdb file is from type: Microsoft C/C++ program database 2.00");
                return new MicrosoftPdbV2Parser(pdbFileBytes);
            } else if (PdbUtils.isPortablePdbV1(pdbFileBytes)) {
                return new PortablePdbV1Parser(pdbFileBytes);
            } else {
                throw new UnsupportedPdbFileException("PDB format is not supported");
            }
        } else {
            throw new PDBParseException("Cannot parse Pdb file, file's content is empty");
        }
    }
}
