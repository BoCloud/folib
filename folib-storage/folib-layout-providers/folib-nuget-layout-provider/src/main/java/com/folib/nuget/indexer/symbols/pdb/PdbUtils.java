package com.folib.nuget.indexer.symbols.pdb;

import java.util.Arrays;

public class PdbUtils {
    public static final String PDB_EXTENSION = ".pdb";
    public static final byte[] MS_PDB_V2_IDENTIFIER = "Microsoft C/C++ program database 2.00\r\n\u001aJG".getBytes();
    public static final byte[] MS_PDB_V7_IDENTIFIER = "Microsoft C/C++ MSF 7.00\r\n\u001aDS".getBytes();
    public static final byte[] PORTABLE_PDB_V1_IDENTIFIER = "BSJB".getBytes();
    public static final String PORTABLE_PDB_SEARCH_SUFFIX = "ffffffff";

    public static boolean isPortablePdbV1(byte[] pdbFileBytes) {
        try {
            if (pdbFileBytes.length < PORTABLE_PDB_V1_IDENTIFIER.length) {
                return false;
            }
            for (int i = 0; i < PORTABLE_PDB_V1_IDENTIFIER.length; i++) {
                if (pdbFileBytes[i] != PORTABLE_PDB_V1_IDENTIFIER[i]) {
                    return false;
                }
            }
            return true;
        } catch (ArrayIndexOutOfBoundsException var2) {
            return false;
        }
    }

    public static boolean isMicrosoftPdbV2(byte[] pdbFileBytes) {
        try {
            if (pdbFileBytes.length < MS_PDB_V2_IDENTIFIER.length) {
                return false;
            }
            for (int i = 0; i < MS_PDB_V2_IDENTIFIER.length; i++) {
                if (pdbFileBytes[i] != MS_PDB_V2_IDENTIFIER[i]) {
                    return false;
                }
            }
            return true;
        } catch (ArrayIndexOutOfBoundsException var2) {
            return false;
        }
    }

    public static boolean isMicrosoftPdbV7(byte[] pdbFileBytes) {
        try {
            if (pdbFileBytes.length < MS_PDB_V7_IDENTIFIER.length) {
                return false;
            }
            for (int i = 0; i < MS_PDB_V7_IDENTIFIER.length; i++) {
                if (pdbFileBytes[i] != MS_PDB_V7_IDENTIFIER[i]) {
                    return false;
                }
            }
            return true;
        } catch (ArrayIndexOutOfBoundsException var2) {
            return false;
        }
    }

    public static String toHexString(short s) {
        return zeropad(Integer.toHexString(shortToInt(s)), 4);
    }

    public static String toHexString(int i) {
        return zeropad(Integer.toHexString(i), 8);
    }

    public static int shortToInt(short s) {
        return s & '\uffff';
    }

    public static String toHexString(byte b) {
        return zeropad(Integer.toHexString(byteToInt(b)), 2);
    }

    public static int byteToInt(byte b) {
        return b & 255;
    }

    public static String zeropad(String s, int len) {
        if (s == null) {
            s = "";
        }

        StringBuilder buffer = new StringBuilder(s);
        int zerosNeeded = len - s.length();

        for(int i = 0; i < zerosNeeded; ++i) {
            buffer.insert(0, '0');
        }

        return buffer.toString();
    }

    public static short getShort(byte[] b, int offset) {
        return (short)((b[offset + 1] & 255) << 8 | b[offset] & 255);
    }

    public static int getInt(byte[] b, int offset) {
        int v = b[offset + 3];

        for(int i = 2; i >= 0; --i) {
            v = v << 8 | b[offset + i] & 255;
        }

        return v;
    }

    public static String bytesAsString(byte[] bytes, int offset, int length) {
        return new String(Arrays.copyOfRange(bytes, offset, offset + length));
    }

    public static int floorDivisionWithLog2Divisor(int dividend, int log2Divisor) {
        return dividend + (1 << log2Divisor) - 1 >> log2Divisor;
    }
}
