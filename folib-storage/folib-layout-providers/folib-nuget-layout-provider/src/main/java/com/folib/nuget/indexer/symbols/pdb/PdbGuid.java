package com.folib.nuget.indexer.symbols.pdb;

import java.util.Arrays;

public class PdbGuid {
    public static final int GUID_STRING_LENGTH = 32;
    private final int data1;
    private final short data2;
    private final short data3;
    private final byte[] data4;

    public PdbGuid(byte[] headerBytes) {
        this.data1 = PdbUtils.getInt(headerBytes, 0);
        this.data2 = PdbUtils.getShort(headerBytes, 4);
        this.data3 = PdbUtils.getShort(headerBytes, 6);
        this.data4 = Arrays.copyOfRange(headerBytes, 8, 16);
    }

    public String getGuidAsString() {
        String var10000 = PdbUtils.toHexString(this.data1);
        String sb = var10000 + PdbUtils.toHexString(this.data2) + PdbUtils.toHexString(this.data3) + PdbUtils.toHexString(this.data4[0]) + PdbUtils.toHexString(this.data4[1]) + PdbUtils.toHexString(this.data4[2]) + PdbUtils.toHexString(this.data4[3]) + PdbUtils.toHexString(this.data4[4]) + PdbUtils.toHexString(this.data4[5]) + PdbUtils.toHexString(this.data4[6]) + PdbUtils.toHexString(this.data4[7]);
        return sb.toUpperCase();
    }
}
