package com.folib.nuget.indexer.symbols.pdb;

import jakarta.annotation.Nullable;
import lombok.Getter;



@Getter
public abstract class PdbParser {
    public static final int INT_SIZE = 4;
    public static final int SHORT_SIZE = 2;
    protected final byte[] FILE_BYTES;
    protected int pdbAge = 1;
    protected PdbGuid guid;

    protected PdbParser(byte[] file_bytes) {
        this.FILE_BYTES = file_bytes;
    }

    protected int parseInt(int offset) {
        return PdbUtils.getInt(this.FILE_BYTES, offset);
    }

    protected short parseShort(int offset) {
        return PdbUtils.getShort(this.FILE_BYTES, offset);
    }

    protected String readBytesAsString(int offset, int length) {
        return PdbUtils.bytesAsString(this.FILE_BYTES, offset, length);
    }

    @Nullable
    public abstract PdbGuid parsePdbAndExtractGuid();
}
