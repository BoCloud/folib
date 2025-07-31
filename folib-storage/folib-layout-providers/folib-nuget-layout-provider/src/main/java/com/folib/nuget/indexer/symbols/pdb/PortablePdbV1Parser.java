package com.folib.nuget.indexer.symbols.pdb;

import com.folib.nuget.indexer.symbols.exception.PDBParseException;
import com.folib.nuget.indexer.symbols.stream.PortablePdbStreamHeader;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import jakarta.annotation.Nullable;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

@Getter
@Slf4j
public class PortablePdbV1Parser extends PdbParser{
    public static final String PDB_STREAM = "#Pdb";
    private static final int VERSION_LENGTH_OFFSET = 12;
    private static final int VERSION_VALUE_OFFSET = 16;
    private static final byte DELIMITER = 0;
    private final Map<String, PortablePdbStreamHeader> streamHeadersMap;
    private int versionLength;
    private short numOfStreams;
    private boolean lazyInit = true;

    public PortablePdbV1Parser(byte[] fileBytes, boolean lazyInit) {
        super(fileBytes);
        this.lazyInit = lazyInit;
        this.streamHeadersMap = new HashMap();
    }

    public PortablePdbV1Parser(byte[] fileBytes) {
        super(fileBytes);
        this.streamHeadersMap = new HashMap();
    }

    private void initStreamHeadersMap(boolean lazyInit) {
        int offset = 16 + this.versionLength + 4;

        for(int i = 0; i < this.numOfStreams; ++i) {
            int headerOffset = this.parseInt(offset);
            offset += 4;
            int streamSize = this.parseInt(offset);
            offset += 4;
            int headerNameSize = this.getStreamHeaderNameSize(offset);
            String headerName = this.readBytesAsString(offset, headerNameSize - 1);
            this.streamHeadersMap.put(headerName, new PortablePdbStreamHeader(headerOffset, streamSize, headerName));
            if (lazyInit && headerName.equals("#Pdb")) {
                return;
            }

            offset = this.getNextStreamHeaderOffset(offset + headerNameSize);
        }

    }

    private int getNextStreamHeaderOffset(int offset) {
        return (int)((double)4.0F * Math.ceil(Math.abs((double)offset / (double)4.0F)));
    }

    private int getStreamHeaderNameSize(int headerNameOffset) {
        int headerNameSize = 0;
        for(int i = headerNameOffset; i < this.FILE_BYTES.length; ++i) {
            if (this.FILE_BYTES[i] == 0) {
                break;
            }
            ++headerNameSize;
        }

        return headerNameSize + 1;
    }

    private void parseRootStream() {
        this.versionLength = this.parseInt(12);
        this.numOfStreams = this.parseShort(16 + this.versionLength + 2);
    }

    @Nullable
    public PdbGuid parsePdbAndExtractGuid() {
        try {
            this.parseRootStream();
            this.initStreamHeadersMap(this.lazyInit);
            PortablePdbStreamHeader mainHeader = (PortablePdbStreamHeader)this.streamHeadersMap.get("#Pdb");
            if (mainHeader == null) {
                log.warn("Could not find '{}' stream header", "#Pdb");
                return null;
            } else {
                int streamOffset = mainHeader.getStreamOffset();
                byte[] guidBytes = Arrays.copyOfRange(this.FILE_BYTES, streamOffset, streamOffset + 16);
                this.guid = new PdbGuid(guidBytes);
                return this.guid;
            }
        } catch (Exception e) {
            log.error("Could not parse Portable PDB because of the following: {}", e.getMessage());
            log.debug("Could not parse Portable PDB because of the following: ", e);
            throw new PDBParseException(e);
        }
    }
}
